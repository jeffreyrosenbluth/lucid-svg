def element(s):
    comment = '-- | @{}@ element\n'.format(s)
    type_sig = '{}_ :: Term arg result => arg -> result\n'.format(s)
    body = '{}_ = term "{}"\n'.format(s,s)
    return comment + type_sig + body

def from_file():
    f = open('elements.txt')
    return f.read().split()

elems = from_file()
for e in elems:
    print element(e)
