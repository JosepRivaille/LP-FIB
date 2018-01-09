import argparse
import re
import ast
import urllib.request
import xml.etree.ElementTree as ElemTree

from datetime import datetime, timedelta
from functools import reduce

AND = 'and'
OR = 'or'

EVENT_URL = 'http://w10.bcn.es/APPS/asiasiacache/peticioXmlAsia?id=199'
MAP_URL = 'http://opendata-ajuntament.barcelona.cat' \
          '/resources/bcn/TRANSPORTS%20GEOXML.xml'


class KeyTree(object):
    def __init__(self, text=None, join=None, children=None):
        self.text = text
        self.join = join
        self.children = children

    def eval_tree(self, info):
        if self.join is AND:
            return all(child.eval_tree(info) for child in self.children)
        elif self.join is OR:
            return any(child.eval_tree(info) for child in self.children)
        else:
            return self.text in info


def get_resource_root(url):
    sock = urllib.request.urlopen(url)
    xml_source = sock.read()
    sock.close()
    return ElemTree.fromstring(xml_source)


def filter_by_key(evs, ktree):
    def contains_key(e):
        p = e.find('lloc_simple')
        a = p.find('adreca_simple')
        event_attributes = ' '.join([e.findtext('nom').lower(),
                                     p.findtext('nom').lower(),
                                     a.findtext('municipi').lower(),
                                     a.findtext('districte').lower()])
        return ktree.eval_tree(event_attributes)

    return list(filter(lambda event: contains_key(event), evs))


def filter_by_date(evs, dates):
    def in_range(ed, d):
        if isinstance(d, tuple):
            prev = d[0].date() - timedelta(days=d[1]) <= ed.date()
            return prev and (d[0].date() + timedelta(days=d[2]) >= ed.date())
        else:
            return d.date() == ed.date()

    def ranged_in_dates(e):
        ed = e.findtext('data/data_proper_acte')[:10]
        ed = datetime.strptime(ed, '%d/%m/%Y')
        return any(in_range(ed, d) for d in dates)

    return list(filter(lambda event: ranged_in_dates(event), evs))


def filter_by_subway(events, subway):
    pass


def interpolate_variables(template, evs):
    regex = r'{{foreach event}}'
    match = re.search(regex, template)

    embedded = ''
    for event in evs:
        place = event.find('lloc_simple')
        address = place.find('adreca_simple')
        date_info = event.find('data')
        data = {
            'name': event.findtext('nom'),
            'place': place.findtext('nom'),
            'street': address.findtext('carrer'),
            'city': address.findtext('municipi'),
            'date': date_info.findtext('data_proper_acte'),
            'hour': date_info.findtext('hora_fi')
        }
        embedded += '<tr>\n\t<td>{name}</td>\n\t<td>{place}, {street}, {city}' \
                    '</td>\n\t<td>{date}, {hour}</td>\n</tr>\n'.format(**data)

    span = match.span()
    html = template.replace(template[span[0]:span[1]], embedded)
    return html


def cast_key(string):
    def build_key_tree(key):
        if isinstance(key, list):
            children = [build_key_tree(child) for child in key]
            return KeyTree(join=AND, children=children)
        elif isinstance(key, tuple):
            children = [build_key_tree(child) for child in key]
            return KeyTree(join=OR, children=children)
        else:
            return KeyTree(key)

    try:
        parsed_key = ast.literal_eval(string.lower())
        return build_key_tree(parsed_key)
    except Exception:
        raise argparse.ArgumentTypeError('Invalid syntax in %r.' % string)


def cast_date(string):
    def join_if_tuple(dates):
        is_tuple = False
        current = None
        result = []
        for date in dates:
            if is_tuple:
                current += ',' + date
                if date[-1] is ')':
                    result.append(current)
                    is_tuple = False
            else:
                if date[0] is '(':
                    is_tuple = True
                    current = date
                else:
                    result.append(date)
        return result

    def build_day_list(dates):
        if dates[0] is '[' and string[-1] is ']':
            date_info = string[1:-1].split(',')
            date_info = join_if_tuple(date_info)
            print(date_info)
            casted_parts = map(cast_date, date_info)
            return reduce(lambda acc, pt: acc + pt, casted_parts)

        elif string[0] is '(' and string[-1] is ')':
            date_info = string[1:-1].split(',')
            current_date = datetime.strptime(date_info[0], '%d/%m/%Y')
            return [(current_date, -int(date_info[1]), int(date_info[2]))]
        else:
            return [datetime.strptime(string, '%d/%m/%Y')]

    try:
        string = string.replace(' ', '')
        return build_day_list(string)
    except Exception:
        raise argparse.ArgumentTypeError('%r format is invalid.' % string)


def cast_metro(string):
    if string[0] != '[' or string[-1] != ']':
        msg = '%r is not a valid list.' % string
        raise argparse.ArgumentTypeError(msg)
    subway_lines = list(map(lambda l: l.strip(), string[1:-1].split(',')))
    if any(re.match(r'^L\d$', line) is None for line in subway_lines):
        msg = 'Each element of %r must be of the format L#.' % string
        raise argparse.ArgumentTypeError(msg)
    return subway_lines


###############
#  Main program
###############

parser = argparse.ArgumentParser(description='Looks for nearby kid activities')

parser.add_argument('--key',
                    type=cast_key,
                    help='Query with conjunctions, disjunctions, and text')
parser.add_argument('--date',
                    type=cast_date,
                    help='dd/mm/yyyy optional interval numeric margin')
parser.add_argument('--metro',
                    type=cast_metro,
                    help='List with subway lines formatted as L#')
args = parser.parse_args()

events = get_resource_root(EVENT_URL).findall('.//acte')
map_info = get_resource_root(MAP_URL)

if args.key is not None:
    events = filter_by_key(events, args.key)
if args.date is not None:
    events = filter_by_date(events, args.date)
# if args.metro is not None:
#     events = filter_by_subway(events, args.metro)

with open('template.html', 'r') as tpl, open('out.html', 'w') as out:
    base_tpl = tpl.read().replace('\n', '').replace('\t', '')
    output_html = interpolate_variables(base_tpl, events)
    out.write(output_html)
