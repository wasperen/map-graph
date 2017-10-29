import csv

class Node:
    def __init__(self, name, place = None):
        self.id = name.lower()
        self.name = name
        self.place = place
    
    def __repr__(self):
        return 'Node[{}]'.format(self.id)
    
    def __eq__(self, other):
        return self.id == other.id if type(other) is Node else self.id == other

class Link:
    def __init__(self, from_node, to_node):
        self.id = from_node.id + "/" + to_node.id
        self.from_node = from_node
        self.to_node = to_node
    
    def __repr__(self):
        return 'Link[{}]'.format(self.id)

with open('map-data.tsv') as in_file:
    reader = csv.reader(in_file, delimiter = '\t')
    next(reader, None) #skip header
    data = [dict(
                from_id = r[1].strip(),
                to_id = r[2].strip(),
                color = int(r[3]),
                __eq__(self, other) = lamdba 
            ) for r in reader]

nodes = set([n for n in map(lambda d: d['from_id'], data) if n != 'base'] + \
    [n for n in map(lambda d: d['to_id'], data) if n != 'base'])
nodes = [Node(n) for n in nodes]

links = [Link(*p) for p in [(nodes[nodes.index(d['from_id'].lower())],\
              nodes[nodes.index(d['to_id'].lower())]) for d in data
                if d['from_id'] != 'base' and d['to_id'] != 'base']]

start_nodes = [node for node in nodes
               if node not in [l.to_node for l in links]]

