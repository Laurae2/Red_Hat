## xgboost_tree.R

Test du gbtree, mais jamais convergé donc abandonné.

Edit: 2500 trees sur tout = 0.991827 Pubilc LB


## xgboost_tree_noleak.R

Version du gbtree avec les 2 features de dates de Laurae et sans le group_1 qui est inutile puisqu'on a découpé par group_1.

Donne 0.994+ sur le premier fold sans aucun tuning, à continuer. Les prédictions sont stackées, et renvoyées dans un fichier CSV.

P.S : je fais confiance à slice.xgb.DMatrix, s'il y a une erreur dedans dans la gestion des indexes...

Exemple :

retrouver la part d'AUC manquante : (70000 * 0.987) / 498686 * 2 = 0.277088 (supposant les non-leakées toutes fausses)

0.97 localement (raddar gbtree) => (0.277*0.97 + (1 - 0.277)) = 0.991687 (identique à ce qu'on voit sur le LB)

0.96 localement (raddar gblinear) => (0.277*0.96 + (1 - 0.277)) = 0.988916 (idem)
