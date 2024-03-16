

# https://www.wikidata.org/wiki/Special:EntityData/Q18644475.json

import urllib.request
import json


# to start, direct this into qs.jsonl; not to be repeated
def get_wikidata_jsonl():
    with open('qs.tmp') as file:
        qids = [line.strip() for line in file]

    for qid in qids:
        try:
            with urllib.request.urlopen('https://www.wikidata.org/wiki/Special:EntityData/'+ qid +'.json') as url:
                data = json.load(url)
                print(json.dumps(data))
        except:
            pass


# then use the generated file to collect labels, direct to qid-lexicon.jsonl
def wikidata2lexicon():
##    lexicon = {}
    with open('qs.jsonl') as file:
        for line in file:
            dict = json.loads(line)
            qids = dict.get('entities', {})
#            print(list(qids.keys()))
            labels = (list(qids.keys())[0], list(qids.values())[0].get('labels', {}))
#            print(labels)
            labdict = {labels[0]: {lab: labels[1][lab]['value'] for lab in labels[1]}}
#            print(labdict)
##            lexicon[labels[0]] = {lab: labels[1][lab]['value'] for lab in labels[1]}
            print(json.dumps(labdict, ensure_ascii=False))
            
# convert 2-letter codes to 3-letter ones
language_codes = {
    'de': 'Ger',
    'en': 'Eng',
    'fi': 'Fin',
    'fr': 'Fre',
    'it': 'Ita',
    'sv': 'Swe'
  }


def mk_gf_fun(s):
    if all(c.isdigit() or c.isalpha() or c in "_'" for c in s):
        return s
    else:
        return "'" + s + "'"
        
            
# use the lexicon to generate GF files
def lexicon2gf(*langs):
    cats = ['QN']
    with open('qid-lexicon.jsonl') as file:
        its = [json.loads(line) for line in file]
    funs = []
    for it in its:
        qid = list(it.keys())[0]
        lins = {lan: it[qid].get(lan, 'NONE').split() for lan in langs}
        en = lins['en']
        # print(qid, en)
        fun = mk_gf_fun('_'.join(en + [qid, 'QN']))
        # print(fun)
        funs.append((qid, fun, lins))
        
    with open('MathWikidata.gf', 'w') as file:
        file.write('abstract MathWikidata = {\n')
        file.write('\n')
        file.write('cat QN ;\n')
        for fun in funs:
            file.write(' '.join(['fun', fun[1], ':', 'QN', ';\n']))
        file.write('}\n')

    for lan in langs:
        lang = language_codes[lan]
        with open('MathWikidata' + lang + '.gf', 'w') as file:
            file.write('concrete MathWikidata' + lang + ' of MathWikidata = \n')
            file.write('open Syntax' + lang + ', Paradigms' + lan + ' in {\n')
            file.write('lincat QN = CN ;\n')
            file.write('oper mkQN = overload {\n')
            for i in range(1, len(max([fun[2][lan] for fun in funs] , key=len))):
                file.write('  mkQN : (' + '_, '*i + '_ : Str) -> QN = mkCN ;\n')
            file.write('  }\n')
            for fun in funs:
                file.write(' '.join(['lin', fun[1], '=', 'mkQN'] + ['"' + w + '"' for w in fun[2][lan]] + [';\n']))
            file.write('}\n')

        


lexicon2gf('en') ## , 'sv', 'fr')






    
