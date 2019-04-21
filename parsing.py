from moz_sql_parser import parse
import json

def parse_sql(sql):
    return json.dumps(parse(sql))
