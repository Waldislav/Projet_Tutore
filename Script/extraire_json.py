import pandas as pd
import json
from pandas import json_normalize

# 1. Charger le fichier .parquet
df = pd.read_parquet('./Data/fichier_unifie.parquet')

# 2. Traiter le champ `pfas_values` (qui est une liste de dictionnaires JSON)
pfas_expanded = df['pfas_values'].apply(json.loads).apply(pd.json_normalize)

# On doit "exploser" la liste pour que chaque élément du tableau soit une ligne distincte, mais sans perdre les autres colonnes
df_expanded = df.explode('pfas_values', ignore_index=True)

# Créer une nouvelle colonne à partir de cette expansion de `pfas_values`
df_pfas = pd.json_normalize(df_expanded['pfas_values'])

# On fusionne les données traitées avec le DataFrame original
df = pd.concat([df_expanded.drop(columns=['pfas_values']), df_pfas], axis=1)

# 3. Traiter le champ `details` (qui est un dictionnaire JSON)
df_details = pd.json_normalize(df['details'].apply(json.loads))

# Fusionner les détails avec le DataFrame original
df = pd.concat([df.drop(columns=['details']), df_details], axis=1)

# 4. Enregistrer à nouveau en format .parquet
df.to_parquet('./Data/Donnees_fin.parquet', engine='pyarrow')

# 5. Enregistrer également en format .xlsx (Excel)
df.to_excel('./Data/Donnees_fin.xlsx', index=False)

print("Traitement terminé. Fichiers .parquet et .xlsx enregistrés dans './Data'.")
