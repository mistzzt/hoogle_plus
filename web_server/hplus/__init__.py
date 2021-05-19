from flask import Flask, Response, stream_with_context, request, json
import uuid
import subprocess
import time
import logging
from flask_cors import CORS, cross_origin
from expiringdict import ExpiringDict
import yaml
from hplus.util import *

cache = ExpiringDict(max_len=100, max_age_seconds=(2 * TIMEOUT))
with open("./dataset.yml", 'r') as stream:
    data = yaml.safe_load(stream)

EXAMPLE_USED = 'unsorted_examples'

def create_app(test_config=None):
    # create and configure the app
    app = Flask(__name__, instance_relative_config=True)
    app.config.from_mapping(
        SECRET_KEY='dev',
    )
    CORS(app)


    if test_config is None:
        # load the instance config, if it exists, when not testing
        app.config.from_pyfile('config.py', silent=True)
    else:
        # load the test config if passed in
        app.config.from_mapping(test_config)

    # ensure the instance folder exists
    try:
        os.makedirs(app.instance_path)
    except OSError:
        pass

    # a simple page that says hello
    @app.route('/')
    def index():
        return 'This is a demo page for hoogle+'

    @app.route('/search/type', methods=['GET', 'POST'])
    def search_type():
        def remove_param_name(s):
            params = map(lambda x: x.split(":"), s.split("->"))
            
            typesOnly = list(map(str.strip, map(lambda xs: xs[1] if len(xs) > 1 else xs[0], params)))
            return " -> ".join(typesOnly)

        obj = json.loads(request.data)
        qid = uuid.uuid1()

        all_candidates = filter(lambda x: remove_param_name(x['query']) == remove_param_name(obj['typeSignature']), data)
        entry_to_json = lambda x: json.dumps({'id': qid, 'candidates': [{'code': x['candidate'], 'examples': x[EXAMPLE_USED]}], 'error': '', 'docs': []}) + '\n'

        cache[str(qid)] = 0
        return Response(map(entry_to_json, all_candidates), mimetype='application/json')

    # @app.route('/search/example', methods=['GET', 'POST'])
    # def search_example():
    #     obj = json.loads(request.data)
    #     # print(obj)
    #     query = {
    #              'query': '??',
    #              'inExamples': obj['facts'],
    #              'inArgNames': obj['argNames']
    #             }
    #     proc = run_hplus([f'--json={json.dumps(query)}',
    #                       f'--search-type={QueryType.search_types.value}'])
    #     return json.jsonify(build_object(QueryType.search_types,
    #                                      communicate_result(proc)))

    @app.route('/stop', methods=['GET', 'POST'])
    def stop():
        # qid = request.get_json()['id']
        # pid = cache.get(qid)
        # print('killing pid', pid, 'with group id', os.getpgid(pid))
        # os.killpg(os.getpgid(pid), signal.SIGKILL)
        return ('', 204)

    # @app.route('/examples', methods=['GET', 'POST'])
    # def get_examples():
    #     obj = json.loads(request.data)
    #     # print(obj)
    #     query = {
    #              'exampleQuery': obj['typeSignature'],
    #              'exampleProgram': obj['candidate'],
    #              'exampleExisting': obj['examples']
    #             }
    #     proc = run_hplus([f'--json={json.dumps(query)}',
    #                       f'--search-type={QueryType.search_examples.value}'])
    #     return json.jsonify(build_object(QueryType.search_examples,
    #                                      communicate_result(proc)))

    # @app.route('/example/code', methods=['GET', 'POST'])
    # def result_for():
    #     obj = json.loads(request.data)
    #     # print(obj)
    #     query = {
    #              'execQuery': obj['typeSignature'],
    #              'execProg': obj['candidate'],
    #              'execArgs': obj['args']
    #             }
    #     proc = run_hplus([f'--json={json.dumps(query)}',
    #                       f'--search-type={QueryType.search_results.value}'])
    #     return json.jsonify(build_object(QueryType.search_results,
    #                                      communicate_result(proc)))
    @app.route('/examples', methods=['GET', 'POST'])
    def get_examples():
        obj = json.loads(request.data)
        # print(obj)
        # query = {
        #          'exampleQuery': obj['typeSignature'],
        #          'exampleProgram': obj['candidate'],
        #          'exampleExisting': obj['examples']
        #         }
        return json.jsonify({
            'examples': [],
            'error': 'No more examples'
        })

    @app.route('/example/code', methods=['GET', 'POST'])
    def result_for():
        obj = json.loads(request.data)
        # print(obj)
        # query = {
        #          'execQuery': obj['typeSignature'],
        #          'execProg': obj['candidate'],
        #          'execArgs': obj['args']
        #         }

        
        return json.jsonify({
            'result': 'N/A',
            'error': 'N/A'
        })

    return app
