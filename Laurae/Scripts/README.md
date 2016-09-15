## xgboost_tree.R

Test du gbtree, mais jamais convergé donc abandonné.

Edit: 2500 trees sur tout = 0.991827 Pubilc LB - modèle dans laurae_brut_2500.model (utiliser xgb.load)


## xgboost_tree_noleak.R

Version du gbtree avec les 2 features de dates de Laurae et sans le group_1 qui est inutile puisqu'on a découpé par group_1.

Donne 0.994+ sur le premier fold sans aucun tuning, à continuer. Les prédictions sont stackées, et renvoyées dans un fichier CSV.

P.S : je fais confiance à slice.xgb.DMatrix, s'il y a une erreur dedans dans la gestion des indexes...

Exemple :

retrouver la part d'AUC manquante : (70000 * 0.987) / 498686 * 2 = 0.277088 (supposant les non-leakées toutes fausses)

0.97 localement (raddar gbtree) => (0.277*0.97 + (1 - 0.277)) = 0.991687 (identique à ce qu'on voit sur le LB)

0.96 localement (raddar gblinear) => (0.277*0.96 + (1 - 0.277)) = 0.988916 (idem)


## xgboost_tree_quad.R

Test du gbtree avec interactions quadratiques forcées (interactions 2-way)

Score : ?????

Liste des interactions:

```
Doing char_1 & char_2. Columns kept: 108  but only 62 interactions.  Total columns: 62.
Doing char_1 & people_char_4. Columns kept: 125  but only 73 interactions.  Total columns: 135.
Doing char_1 & people_char_7. Columns kept: 162  but only 110 interactions.  Total columns: 245.
Doing char_1 & char_9. Columns kept: 127  but only 81 interactions.  Total columns: 326.
Doing char_1 & char_8. Columns kept: 108  but only 64 interactions.  Total columns: 390.
Doing char_1 & char_3. Columns kept: 105  but only 68 interactions.  Total columns: 458.
Doing char_1 & char_7. Columns kept: 93  but only 57 interactions.  Total columns: 515.
Doing char_1 & people_char_5. Columns kept: 101  but only 65 interactions.  Total columns: 580.
Doing char_1 & people_char_9. Columns kept: 96  but only 60 interactions.  Total columns: 640.
Doing char_1 & char_4. Columns kept: 79  but only 45 interactions.  Total columns: 685.
Doing char_1 & char_5. Columns kept: 83  but only 49 interactions.  Total columns: 734.
Doing char_1 & people_char_8. Columns kept: 91  but only 56 interactions.  Total columns: 790.
Doing char_1 & activity_category. Columns kept: 61  but only 27 interactions.  Total columns: 817.
Doing char_1 & people_char_6. Columns kept: 84  but only 50 interactions.  Total columns: 867.
Doing char_1 & char_6. Columns kept: 79  but only 46 interactions.  Total columns: 913.
Doing char_1 & people_char_2. Columns kept: 58  but only 28 interactions.  Total columns: 941.
Doing people_char_3 & char_2. Columns kept: 127  but only 74 interactions.  Total columns: 1015.
Doing people_char_3 & people_char_4. Columns kept: 251  but only 192 interactions.  Total columns: 1207.
Doing people_char_3 & people_char_7. Columns kept: 419  but only 360 interactions.  Total columns: 1567.
Doing people_char_3 & char_9. Columns kept: 148  but only 95 interactions.  Total columns: 1662.
Doing people_char_3 & char_8. Columns kept: 128  but only 77 interactions.  Total columns: 1739.
Doing people_char_3 & char_3. Columns kept: 113  but only 69 interactions.  Total columns: 1808.
Doing people_char_3 & char_7. Columns kept: 106  but only 63 interactions.  Total columns: 1871.
Doing people_char_3 & people_char_5. Columns kept: 80  but only 37 interactions.  Total columns: 1908.
Doing people_char_3 & people_char_9. Columns kept: 205  but only 162 interactions.  Total columns: 2070.
Doing people_char_3 & char_4. Columns kept: 96  but only 55 interactions.  Total columns: 2125.
Doing people_char_3 & char_5. Columns kept: 93  but only 52 interactions.  Total columns: 2177.
Doing people_char_3 & people_char_8. Columns kept: 185  but only 143 interactions.  Total columns: 2320.
Doing people_char_3 & activity_category. Columns kept: 153  but only 112 interactions.  Total columns: 2432.
Doing people_char_3 & people_char_6. Columns kept: 149  but only 108 interactions.  Total columns: 2540.
Doing people_char_3 & char_6. Columns kept: 85  but only 45 interactions.  Total columns: 2585.
Doing people_char_3 & people_char_2. Columns kept: 92  but only 55 interactions.  Total columns: 2640.
Doing char_2 & people_char_4. Columns kept: 109  but only 66 interactions.  Total columns: 2706.
Doing char_2 & people_char_7. Columns kept: 147  but only 104 interactions.  Total columns: 2810.
Doing char_2 & char_9. Columns kept: 98  but only 61 interactions.  Total columns: 2871.
Doing char_2 & char_8. Columns kept: 103  but only 68 interactions.  Total columns: 2939.
Doing char_2 & char_3. Columns kept: 88  but only 60 interactions.  Total columns: 2999.
Doing char_2 & char_7. Columns kept: 76  but only 49 interactions.  Total columns: 3048.
Doing char_2 & people_char_5. Columns kept: 93  but only 66 interactions.  Total columns: 3114.
Doing char_2 & people_char_9. Columns kept: 74  but only 47 interactions.  Total columns: 3161.
Doing char_2 & char_4. Columns kept: 70  but only 45 interactions.  Total columns: 3206.
Doing char_2 & char_5. Columns kept: 60  but only 35 interactions.  Total columns: 3241.
Doing char_2 & people_char_8. Columns kept: 71  but only 45 interactions.  Total columns: 3286.
Doing char_2 & activity_category. Columns kept: 43  but only 18 interactions.  Total columns: 3304.
Doing char_2 & people_char_6. Columns kept: 69  but only 44 interactions.  Total columns: 3348.
Doing char_2 & char_6. Columns kept: 62  but only 38 interactions.  Total columns: 3386.
Doing char_2 & people_char_2. Columns kept: 45  but only 24 interactions.  Total columns: 3410.
Doing people_char_4 & people_char_7. Columns kept: 354  but only 305 interactions.  Total columns: 3715.
Doing people_char_4 & char_9. Columns kept: 124  but only 81 interactions.  Total columns: 3796.
Doing people_char_4 & char_8. Columns kept: 102  but only 61 interactions.  Total columns: 3857.
Doing people_char_4 & char_3. Columns kept: 100  but only 66 interactions.  Total columns: 3923.
Doing people_char_4 & char_7. Columns kept: 88  but only 55 interactions.  Total columns: 3978.
Doing people_char_4 & people_char_5. Columns kept: 118  but only 85 interactions.  Total columns: 4063.
Doing people_char_4 & people_char_9. Columns kept: 158  but only 125 interactions.  Total columns: 4188.
Doing people_char_4 & char_4. Columns kept: 72  but only 41 interactions.  Total columns: 4229.
Doing people_char_4 & char_5. Columns kept: 78  but only 47 interactions.  Total columns: 4276.
Doing people_char_4 & people_char_8. Columns kept: 142  but only 110 interactions.  Total columns: 4386.
Doing people_char_4 & activity_category. Columns kept: 124  but only 93 interactions.  Total columns: 4479.
Doing people_char_4 & people_char_6. Columns kept: 120  but only 89 interactions.  Total columns: 4568.
Doing people_char_4 & char_6. Columns kept: 69  but only 39 interactions.  Total columns: 4607.
Doing people_char_4 & people_char_2. Columns kept: 65  but only 38 interactions.  Total columns: 4645.
Doing people_char_7 & char_9. Columns kept: 173  but only 130 interactions.  Total columns: 4775.
Doing people_char_7 & char_8. Columns kept: 131  but only 90 interactions.  Total columns: 4865.
Doing people_char_7 & char_3. Columns kept: 147  but only 113 interactions.  Total columns: 4978.
Doing people_char_7 & char_7. Columns kept: 133  but only 100 interactions.  Total columns: 5078.
Doing people_char_7 & people_char_5. Columns kept: 221  but only 188 interactions.  Total columns: 5266.
Doing people_char_7 & people_char_9. Columns kept: 212  but only 179 interactions.  Total columns: 5445.
Doing people_char_7 & char_4. Columns kept: 99  but only 68 interactions.  Total columns: 5513.
Doing people_char_7 & char_5. Columns kept: 93  but only 62 interactions.  Total columns: 5575.
Doing people_char_7 & people_char_8. Columns kept: 189  but only 157 interactions.  Total columns: 5732.
Doing people_char_7 & activity_category. Columns kept: 132  but only 101 interactions.  Total columns: 5833.
Doing people_char_7 & people_char_6. Columns kept: 60  but only 29 interactions.  Total columns: 5862.
Doing people_char_7 & char_6. Columns kept: 97  but only 67 interactions.  Total columns: 5929.
Doing people_char_7 & people_char_2. Columns kept: 75  but only 48 interactions.  Total columns: 5977.
Doing char_9 & char_8. Columns kept: 81  but only 46 interactions.  Total columns: 6023.
Doing char_9 & char_3. Columns kept: 88  but only 60 interactions.  Total columns: 6083.
Doing char_9 & char_7. Columns kept: 91  but only 64 interactions.  Total columns: 6147.
Doing char_9 & people_char_5. Columns kept: 121  but only 94 interactions.  Total columns: 6241.
Doing char_9 & people_char_9. Columns kept: 85  but only 58 interactions.  Total columns: 6299.
Doing char_9 & char_4. Columns kept: 78  but only 53 interactions.  Total columns: 6352.
Doing char_9 & char_5. Columns kept: 77  but only 52 interactions.  Total columns: 6404.
Doing char_9 & people_char_8. Columns kept: 82  but only 56 interactions.  Total columns: 6460.
Doing char_9 & activity_category. Columns kept: 43  but only 18 interactions.  Total columns: 6478.
Doing char_9 & people_char_6. Columns kept: 71  but only 46 interactions.  Total columns: 6524.
Doing char_9 & char_6. Columns kept: 70  but only 46 interactions.  Total columns: 6570.
Doing char_9 & people_char_2. Columns kept: 52  but only 31 interactions.  Total columns: 6601.
Doing char_8 & char_3. Columns kept: 66  but only 40 interactions.  Total columns: 6641.
Doing char_8 & char_7. Columns kept: 69  but only 44 interactions.  Total columns: 6685.
Doing char_8 & people_char_5. Columns kept: 94  but only 69 interactions.  Total columns: 6754.
Doing char_8 & people_char_9. Columns kept: 74  but only 49 interactions.  Total columns: 6803.
Doing char_8 & char_4. Columns kept: 73  but only 50 interactions.  Total columns: 6853.
Doing char_8 & char_5. Columns kept: 62  but only 39 interactions.  Total columns: 6892.
Doing char_8 & people_char_8. Columns kept: 67  but only 43 interactions.  Total columns: 6935.
Doing char_8 & activity_category. Columns kept: 39  but only 16 interactions.  Total columns: 6951.
Doing char_8 & people_char_6. Columns kept: 67  but only 44 interactions.  Total columns: 6995.
Doing char_8 & char_6. Columns kept: 64  but only 42 interactions.  Total columns: 7037.
Doing char_8 & people_char_2. Columns kept: 43  but only 24 interactions.  Total columns: 7061.
Doing char_3 & char_7. Columns kept: 58  but only 40 interactions.  Total columns: 7101.
Doing char_3 & people_char_5. Columns kept: 78  but only 60 interactions.  Total columns: 7161.
Doing char_3 & people_char_9. Columns kept: 70  but only 52 interactions.  Total columns: 7213.
Doing char_3 & char_4. Columns kept: 49  but only 33 interactions.  Total columns: 7246.
Doing char_3 & char_5. Columns kept: 56  but only 40 interactions.  Total columns: 7286.
Doing char_3 & people_char_8. Columns kept: 63  but only 46 interactions.  Total columns: 7332.
Doing char_3 & activity_category. Columns kept: 25  but only 9 interactions.  Total columns: 7341.
Doing char_3 & people_char_6. Columns kept: 49  but only 33 interactions.  Total columns: 7374.
Doing char_3 & char_6. Columns kept: 41  but only 26 interactions.  Total columns: 7400.
Doing char_3 & people_char_2. Columns kept: 29  but only 17 interactions.  Total columns: 7417.
Doing char_7 & people_char_5. Columns kept: 62  but only 45 interactions.  Total columns: 7462.
Doing char_7 & people_char_9. Columns kept: 56  but only 39 interactions.  Total columns: 7501.
Doing char_7 & char_4. Columns kept: 37  but only 22 interactions.  Total columns: 7523.
Doing char_7 & char_5. Columns kept: 47  but only 32 interactions.  Total columns: 7555.
Doing char_7 & people_char_8. Columns kept: 52  but only 36 interactions.  Total columns: 7591.
Doing char_7 & activity_category. Columns kept: 23  but only 8 interactions.  Total columns: 7599.
Doing char_7 & people_char_6. Columns kept: 43  but only 28 interactions.  Total columns: 7627.
Doing char_7 & char_6. Columns kept: 33  but only 19 interactions.  Total columns: 7646.
Doing char_7 & people_char_2. Columns kept: 26  but only 15 interactions.  Total columns: 7661.
Doing people_char_5 & people_char_9. Columns kept: 81  but only 64 interactions.  Total columns: 7725.
Doing people_char_5 & char_4. Columns kept: 51  but only 36 interactions.  Total columns: 7761.
Doing people_char_5 & char_5. Columns kept: 52  but only 37 interactions.  Total columns: 7798.
Doing people_char_5 & people_char_8. Columns kept: 72  but only 56 interactions.  Total columns: 7854.
Doing people_char_5 & activity_category. Columns kept: 54  but only 39 interactions.  Total columns: 7893.
Doing people_char_5 & people_char_6. Columns kept: 57  but only 42 interactions.  Total columns: 7935.
Doing people_char_5 & char_6. Columns kept: 38  but only 24 interactions.  Total columns: 7959.
Doing people_char_5 & people_char_2. Columns kept: 25  but only 14 interactions.  Total columns: 7973.
Doing people_char_9 & char_4. Columns kept: 47  but only 32 interactions.  Total columns: 8005.
Doing people_char_9 & char_5. Columns kept: 45  but only 30 interactions.  Total columns: 8035.
Doing people_char_9 & people_char_8. Columns kept: 44  but only 28 interactions.  Total columns: 8063.
Doing people_char_9 & activity_category. Columns kept: 50  but only 35 interactions.  Total columns: 8098.
Doing people_char_9 & people_char_6. Columns kept: 55  but only 40 interactions.  Total columns: 8138.
Doing people_char_9 & char_6. Columns kept: 39  but only 25 interactions.  Total columns: 8163.
Doing people_char_9 & people_char_2. Columns kept: 27  but only 16 interactions.  Total columns: 8179.
Doing char_4 & char_5. Columns kept: 28  but only 15 interactions.  Total columns: 8194.
Doing char_4 & people_char_8. Columns kept: 43  but only 29 interactions.  Total columns: 8223.
Doing char_4 & activity_category. Columns kept: 19  but only 6 interactions.  Total columns: 8229.
Doing char_4 & people_char_6. Columns kept: 32  but only 19 interactions.  Total columns: 8248.
Doing char_4 & char_6. Columns kept: 31  but only 19 interactions.  Total columns: 8267.
Doing char_4 & people_char_2. Columns kept: 21  but only 12 interactions.  Total columns: 8279.
Doing char_5 & people_char_8. Columns kept: 42  but only 28 interactions.  Total columns: 8307.
Doing char_5 & activity_category. Columns kept: 19  but only 6 interactions.  Total columns: 8313.
Doing char_5 & people_char_6. Columns kept: 30  but only 17 interactions.  Total columns: 8330.
Doing char_5 & char_6. Columns kept: 29  but only 17 interactions.  Total columns: 8347.
Doing char_5 & people_char_2. Columns kept: 21  but only 12 interactions.  Total columns: 8359.
Doing people_char_8 & activity_category. Columns kept: 44  but only 30 interactions.  Total columns: 8389.
Doing people_char_8 & people_char_6. Columns kept: 50  but only 36 interactions.  Total columns: 8425.
Doing people_char_8 & char_6. Columns kept: 35  but only 22 interactions.  Total columns: 8447.
Doing people_char_8 & people_char_2. Columns kept: 24  but only 14 interactions.  Total columns: 8461.
Doing activity_category & people_char_6. Columns kept: 41  but only 28 interactions.  Total columns: 8489.
Doing activity_category & char_6. Columns kept: 17  but only 5 interactions.  Total columns: 8494.
Doing activity_category & people_char_2. Columns kept: 20  but only 11 interactions.  Total columns: 8505.
Doing people_char_6 & char_6. Columns kept: 30  but only 18 interactions.  Total columns: 8523.
Doing people_char_6 & people_char_2. Columns kept: 19  but only 10 interactions.  Total columns: 8533.
Doing char_6 & people_char_2. Columns kept: 16  but only 8 interactions.  Total columns: 8541.
```
