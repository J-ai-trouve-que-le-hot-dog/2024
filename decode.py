ll = '''abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!"#$%&'()*+,-./:;<=>?@[\]^_`|~ \n'''

encode = {}
for c in range(33,127):
  encode[ll[c-33]] = chr(c)

import sys

if len(sys.argv) > 1:
  for line in sys.stdin:
    print("".join(encode[c] for c in line))
else:
  for line in sys.stdin:
    print("".join(ll[ord(c)-33] for c in line.strip()))
