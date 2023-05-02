# Documentation du paquet `ratlas`

`ratlas` est un paquet R permettant l'accès et l'analyse des données de biodiversité aggrégée par le réseau d'observation de la biodiversité du Québec.

Ce paquet R expose les services `RESTFull` de l'API de Atlas. Atlas est un système d'information sur la biodiversité du Québec développé par le laboratoire d'Écologie Intégrative de l'Université de Sherbrooke.

La documentation complète du package est disponible [ici](https://ReseauBiodiversiteQuebec.github.io/ratlas).

Pour débuter avec le package `ratlas` nous vous recommendons l'article pour
débuter par le [tutoriel pour le téléchargement d'observations](https://ReseauBiodiversiteQuebec.github.io/ratlas/articles/download-obs.html).

## Installer le paquet `ratlas`

```r
devtools::install_github("ReseauBiodiversiteQuebec/ratlas")
```

## S'authentifier auprès de l'API

Un jeton d'accès vous sera mis à disposition par l'équipe de développeur de Atlas. Veuillez les contacter pour en obtenir un de manière sécurisée.

Il est **fortement recommandé** de mettre en cache votre jeton d'accès (jeton d'accès stocké dans un fichier `rds`) afin de s'assurer qu'il ne soit pas visible ou transmis avec votre code à un autre utilisateur. Ce jeton d'accès est unique et révocable. 

Pour cela, il vous suffit simplement d'enregistrer le jeton d'accès directement en tant que _Environment variable_

### Stratégie 1: Mise en cache dans les variables d'environnement de R

```r
file.edit("~/.Renviron")
```

Cette ligne va ouvrir un ficher text dans votre Rstudio. Rajoutez dans ce ficher un linge comme la suivante:


```r
# utilizez votre propre token ici

ATLAS_API_TOKEN=7f8df438e1be96a18436e9dab5d97d68ed0e0441d9b68f59e0ce631b2919f3aa
```

*Le jeton d'accès est un exemple ici et n'est aucunement valide.*

### Stratégie 2: Environnement au niveau OS / runtime

Vous pouvez également passer votre jeton d'accès en créant une variable d'environnement nommée `ATLAS_API_TOKEN` au niveau de l'OS ou de l'environnement de dévelopement.


## Troubleshooting

### Erreur 401 : Unauthorized

Si vous obtenez une erreur 401, c'est que votre jeton d'accès n'est pas valide. Veuillez vous assurer que vous avez bien copié le jeton d'accès et que vous l'avez bien mis en cache dans votre environnement de développement.

Pour valider que votre jeton d'accès est bien en cache, vous pouvez utiliser la fonction `Sys.getenv("ATLAS_API_TOKEN")` qui devrait retourner votre jeton d'accès.