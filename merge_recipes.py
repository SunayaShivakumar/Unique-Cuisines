import os
rfiles = os.listdir("/Users/sunayakumar/Desktop/project/recipes")
print (rfiles)
rc = []
for f in rfiles:
    if '.txt' in f: 
        infile = open(os.path.join("/Users/sunayakumar/Desktop/project/recipes", f), 'r')
        rc.append(infile.read())
        infile.close()
all_rs = '\n'.join(rc)
import re
line_pat = re.compile('[A-Za-z]+\t.+\n')
recipe_lines = line_pat.findall(all_rs)
new_recipe_lines = []
cuisine_lines = []
for n,r in enumerate(recipe_lines):
    cuisine = r[:r.find('\t')]
    new_recipe_lines.append(recipe_lines[n].replace(cuisine, ''))
    cuisine_lines.append(cuisine + '\n')
outfile1 = open('recipes combined.tsv', 'wb')
outfile1.write(''.join(new_recipe_lines))
outfile1.close()
outfile2 = open('cuisines.csv', 'wb')
outfile2.write(''.join(cuisine_lines))
outfile2.close()