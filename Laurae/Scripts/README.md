## xgboost_tree.R

Test du gbtree, mais jamais convergé donc abandonné.


## xgboost_tree_noleak.R

Version du gbtree avec les 2 features de dates de Laurae et sans le group_1 qui est inutile puisqu'on a découpé par group_1.

Donne 0.994+ sur le premier fold sans aucun tuning, à continuer. Les prédictions sont stackées, et renvoyées dans un fichier CSV.

P.S : je fais confiance à slice.xgb.DMatrix, s'il y a une erreur dedans dans la gestion des indexes...
