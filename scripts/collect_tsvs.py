#!/usr/bin/python3

import re
import os
import csv
import json
from tabulate import tabulate

DATA_DIR = "tmp/results"
OUTPUT_FILE = "collected.csv"
name_regex = r'(.*)\+(.*)\.tsv'

TYGARQ = "TYGAR-Q"
TYGARQB5 = "TYGAR-QB-5"
TYGAR0 = "TYGAR-0"
TYGAR0B5 = "TYGAR-0B-5"
NOGAR = "NoGar"
SYPET = "Sypet-Clone"

def gather_files():
    files = os.listdir(DATA_DIR)
    per_query = {}
    for file_name in files:
        match = re.search(name_regex, file_name)
        if not match:
            continue
        # assume we have a match
        query_name = match.group(2)
        exp_name = match.group(1)
        if query_name not in per_query:
            per_query[query_name] = {}
        per_query[query_name][exp_name] = []
        file_path = os.path.join(DATA_DIR, file_name)
        with open(file_path, 'r') as f:
            reader = csv.DictReader(f, delimiter="\t")
            per_query[query_name][exp_name] = list(reader)
    return per_query

def mk_table(per_query):
    table = []
    keys_list = list(per_query.keys())
    for idx in range(len(keys_list)):
        query_group = per_query[keys_list[idx]]
        table.append(mk_row(idx, query_group))
    return table

def header():
    return [
        "N",
        "Query",
        "t-Q",
        "t-QB5",
        "t-0",
        "t-0B5",
        "t-NO",
        "t-Sypet",
        "ct-Q",
        "ct-0",
        "ct-NO",
        "ct-Sypet",
        "st-Q",
        "st-0",
        "st-NO",
        "st-Sypet",
        "tc-Q",
        "tc-0",
        "tc-NO",
        "tc-Sypet",
        "tr-Q",
        "tr-QB5",
        "tr-0",
        "tr-0B5",
        "tr-NO",
        "tr-Sypet",
    ]

def mk_row(idx, query_group):
    return [
        idx,
        safeGetFirst(query_group, TYGARQ, "query"),
        safeGetFirst(query_group, TYGARQ, "totalTime"),
        safeGetFirst(query_group, TYGARQB5, "totalTime"),
        safeGetFirst(query_group, TYGAR0, "totalTime"),
        safeGetFirst(query_group, TYGAR0B5, "totalTime"),
        safeGetFirst(query_group, NOGAR, "totalTime"),
        safeGetFirst(query_group, SYPET, "totalTime"),
        safeGetFirst(query_group, TYGARQ, "constructionTime"),
        safeGetFirst(query_group, TYGAR0, "constructionTime"),
        safeGetFirst(query_group, NOGAR, "constructionTime"),
        safeGetFirst(query_group, SYPET, "constructionTime"),
        safeGetFirst(query_group, TYGARQ, "solverTime"),
        safeGetFirst(query_group, TYGAR0, "solverTime"),
        safeGetFirst(query_group, NOGAR, "solverTime"),
        safeGetFirst(query_group, SYPET, "solverTime"),
        safeGetFirst(query_group, TYGARQ, "typeCheckerTime"),
        safeGetFirst(query_group, TYGAR0, "typeCheckerTime"),
        safeGetFirst(query_group, NOGAR, "typeCheckerTime"),
        safeGetFirst(query_group, SYPET, "typeCheckerTime"),
        get_transitions(query_group, TYGARQ),
        get_transitions(query_group, TYGARQB5),
        get_transitions(query_group, TYGAR0),
        get_transitions(query_group, TYGAR0B5),
        get_transitions(query_group, NOGAR),
        get_transitions(query_group, SYPET),
    ]

def write_csv(table):
    with open(OUTPUT_FILE, "w") as f:
        writer = csv.writer(f)
        writer.writerow(header())
        for row in table:
            writer.writerow(row)

def write_latex(table):
    with open("result.tex", "w") as f:
        f.write(tabulate(table, header(), tablefmt="latex"))


def safeGetFirst(dct, exp, field):
    try:
        return dct.get(exp)[0].get(field)
    except (IndexError, KeyError, TypeError):
        return None

def get_transitions(dct, exp):
    try:
        safeGetFirst(dct, exp, "numOfTransitions")[-1]
    except (IndexError, TypeError):
        return None

def main():
    per_query = gather_files()
    table = mk_table(per_query)
    write_csv(table)
    write_latex(table)

if __name__ == '__main__':
    main()