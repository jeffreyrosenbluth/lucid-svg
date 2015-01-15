def attribute(s):
    comment = '-- | The @{}@ attribute.\n'.format(s)
    type_sig = '{}_ :: Text -> Attribute\n'.format(s)
    body = '{}_ = makeAttribute "{}"\n'.format(s,s)
    return comment + type_sig + body

def from_file():
    f = open('attributes.txt')
    return f.read().split()

attrs = from_file()
for a in attrs:
    print attribute(a)
