import os
import re
import json
import yaml
import numpy as np
import pandas as pd
import matplotlib as mpl
from glob import glob
from matplotlib import cm
from matplotlib.ticker import FuncFormatter
from synthesis import run_synthesis

FILE_TO_CLS = os.path.join(os.path.dirname(__file__), '../benchmark/suite/classification.yml')
MODULE_NAME_RE = re.compile(r"[\w+\.\w+]+\.([\S]+)(?<!\))")

def remove_module_name(n):
    for r in MODULE_NAME_RE.finditer(n):
        n = n.replace(r.group(0), r.group(1))
    return n

def extract_solution_from_raw_result(result):
    if not result['result']:
        return []

    return list(map(lambda obj: remove_module_name(obj[0]['solution']),
                    result['result']))

def extract_result(result):
    return {
        'name': result['name'],
        'query': result['query'],
        'candidate': extract_solution_from_raw_result(result),
        'examples': []
    }

def load_experiment_result(dir_name):
    '''Load the experiment result from a given directory.'''
    files = glob(join(dir_name, "*.json"))

    raw_results = []
    for file_name in files:
        with open(file_name) as file: raw_results.append(json.load(file))

    return list(map(extract_result, raw_results))

def count_num_class(class_data, invalid_data, xs):
    num_class = 0
    xs_ = set(xs) - set(invalid_data)
    for cls in class_data:
        diff = xs_ - set(cls)
        if len(diff) != len(xs_): # new class found
            num_class += 1
        xs_ = diff

    if len(xs_) != 0:
        print(f"fill it later: {xs_}")
    return num_class

def count_num_invalid(invalid_data, xs):
    return len(set(invalid_data).intersection(xs))

def count_misclassifications(row, class_data, invalid_data):
    universal_candidates = row["360-no-filter"]
    filtered_candidates = row["180-filter"]

    if not filtered_candidates: return 0 # all due to timeout?
    last_in_universal = ""
    for c in filtered_candidates:
        if c in universal_candidates: last_in_universal = c

    if last_in_universal: return 0 # impossible to classify since they don't share common candidate

    universal_candidates = universal_candidates[:universal_candidates.index(last_in_universal)+1]
    filtered_candidates = universal_candidates[:filtered_candidates.index(last_in_universal)+1]

    return count_num_class(class_data, invalid_data, universal_candidates) - count_num_class(class_data, invalid_data, filtered_candidates)

def compute_filter_stats(classifications, row):
    # compute misclassification and timeout

    # timeout: not in 180-filter, in 180-no-filter, in 360-filter

    # misclss: not in 180-filter, not in 360-no-filter, in 180-filter
    # misclss: classify 180-no-filter first, and count number of classes.

    # compute other stats: all synthesized solution, invalids, duplicates, interesting solution
    # since sometimes 180-no-filter include garbage, we use 360-no-filter

    universal_candidates = set(row["360-no-filter"])
    subset_candidates = set(row["180-no-filter"])

    universal_filtered_candidates = set(row["360-filter"])
    subset_filtered_candidates = set(row["180-filter"])

    classification = classifications.loc[row.name[0]]
    cls_invalid = classification["invalid"]
    cls_classes = classification["class"]

    num_all_candidates = len(set(subset_candidates))
    num_invalid_candidates = count_num_invalid(cls_invalid, subset_candidates)
    num_all_classes = count_num_class(cls_classes, cls_invalid, subset_candidates)
    num_all_filtered_classes = len(subset_filtered_candidates)
    num_all_duplicates = num_all_candidates - num_invalid_candidates - num_all_classes

    num_timeout = len(universal_filtered_candidates.intersection(subset_candidates) - subset_filtered_candidates)
    num_misclss = count_misclassifications(row, cls_classes, cls_invalid)

    row['num_timeout'] = num_timeout
    row['num_misclss'] = num_misclss
    row['num_all_candidates'] = num_all_candidates
    row['num_invalid_candidates'] = num_invalid_candidates
    row['num_all_duplicates'] = num_all_duplicates
    row['num_interesting'] = num_all_candidates - num_invalid_candidates - num_all_duplicates

    return row

def plot_graph(value_df):
    # prepare the percentages
    graph_df = value_df[['num_timeout', 'num_misclss', 'num_all_candidates', 'num_invalid_candidates', 'num_all_duplicates']]
    graph_df['loss_timeout'] = graph_df['num_timeout'] / graph_df['num_all_candidates']
    graph_df['loss_misclss'] = graph_df['num_misclss'] / graph_df['num_all_candidates']
    graph_df['invalids'] = graph_df['num_all_duplicates'] / graph_df['num_all_candidates']
    graph_df['useful'] = 1 - graph_df['invalids'] - graph_df['loss_misclss'] - graph_df['loss_timeout']
    graph_df.sort_values(by=['useful'], inplace=True)
    graph_df = graph_df.reset_index().set_index('name').drop(['containsEdge'], axis=0)

    # plot the histogram
    bar_df_ = graph_df[['useful', 'loss_timeout', 'loss_misclss', 'invalids']]
    ax = bar_df_.plot.bar(stacked=True, figsize=(14, 7),
                          color=['#51127c', '#fc8961', '#b73779','gainsboro'])
    ax.yaxis.set_major_formatter(FuncFormatter(lambda x, _: '{:.0%}'.format(x)))
    # ax.xaxis.set_visible(False)
    ax.axhline(1, color='k', linestyle='--')
    ax.legend(labels=['All H+ Solutions',
                      'Interesting Solutions',
                      'Loss by Timeout',
                      'Loss by Misclassification',
                      'Duplicates and Crashings'], loc='upper left')
    ax.set_ylabel('Percents of all generated solutions')
    plt.save_fig(os.path.join(output_dir, 'filtering.png'))

def run_filtering(groups, output_dir):
    experiments = [('180-filter', ['--disable-filter=False', '--cnt=10'], 180),
                   ('180-no-filter', ['--disable-filter=True', '--cnt=10'], 180),
                   ('360-filter', ['--disable-filter=False', '--cnt=30'], 360),
                   ('360-no-filter', ['--disable-filter=True', '--cnt=30'], 360)]

    # run the experiment and store the returns
    dfs = []
    for expr, options, t in experiments:
        # create the directories if needed
        exp_dir = os.path.join(output_dir, expr)
        if not os.path.isdir(exp_dir):
            os.makedirs(exp_dir)

        run_synthesis(groups, exp_dir, t, True)

        df = pd.DataFrame(load_experiment_result(exp_dir)).set_index(["name", "query"])
        dfs.append(df[["candidate"]].rename({"candidate": name},
                                            axis="columns"))
    exp_df = pd.concat(dfs, axis=1, sort=False)

    # load the manual classification from the file
    with open(file_to_cls, 'r') as f:
        classifications = yaml.load(f, Loader=yaml.FullLoader)
    classifications = pd.DataFrame(classifications).set_index('name')

    value_df = exp_df.apply(lambda r: compute_filter_stats(classifications, r),
                             axis=1)

    plot_graph(value_df)
