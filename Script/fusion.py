import pandas as pd

csv1 = pd.read_csv('ades.csv')
csv2 = pd.read_csv('naiades.csv')

fusion_csv = pd.concat([csv1, csv2], ignore_index=True)

fusion_csv.to_csv('france.csv', index=False)
