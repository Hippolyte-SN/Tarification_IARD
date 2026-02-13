# Tarification en assurance IARD – Automobile

## 🧭 Description
Ce projet vise à construire un modèle de tarification en assurance automobile
en estimant la prime pure à partir de la fréquence et de la sévérité des sinistres.
Les modèles utilisés reposent sur des GLM, standards en tarification IARD.

---

## 🎯 Objectifs
- Analyser un portefeuille automobile
- Modéliser la fréquence et le cout des sinistres
- Construire une prime pure individuelle
- Interpréter les relativités tarifaires

---

## 📂 Données
Jeu de données représentant un portefeuille automobile.

### Variables principales
- `age`
- `Sexe`
- `Type.vehicule`
- `Date.sous`
- `anc.permis`
- `Zone.geo`
- `valeur`
- `exposition`
- `charge.sin`
- `nb_sinistres`

---

## ⚙️ Méthodologie

### Fréquence
- GLM Poisson
- offset = log(exposition)

### Sévérité
- GLM Log.normal
- lien logarithmique

### Prime pure
\[
\text{Prime pure} = \mathbb{E}[N] \times \mathbb{E}[C]
\]

---

