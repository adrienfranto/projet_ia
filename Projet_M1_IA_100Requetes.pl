% =====================================================
% 100 REQUETES PROLOG POUR SYSTEME D'ENQUETE POLICIERE
% =====================================================

% REQUETES BASIQUES - VERIFICATION D'EXISTENCE (1-20)
% =====================================================

% 1. Vérifier si John est un suspect
?- suspect(john).

% 2. Vérifier si Mary est un suspect
?- suspect(mary).

% 3. Vérifier si Alice est un suspect
?- suspect(alice).

% 4. Vérifier si Bruno est un suspect
?- suspect(bruno).

% 5. Vérifier si Sophie est un suspect
?- suspect(sophie).

% 6. Vérifier si l'assassinat est un type de crime
?- crime_type(assassinat).

% 7. Vérifier si le vol est un type de crime
?- crime_type(vol).

% 8. Vérifier si l'escroquerie est un type de crime
?- crime_type(escroquerie).

% 9. Vérifier si John a un motif pour le vol
?- has_motive(john, vol).

% 10. Vérifier si Mary a un motif pour l'assassinat
?- has_motive(mary, assassinat).

% 11. Vérifier si Alice était près de la scène de crime pour l'escroquerie
?- was_near_crime_scene(alice, escroquerie).

% 12. Vérifier si Bruno a des transactions bancaires liées à l'escroquerie
?- has_bank_transaction(bruno, escroquerie).

% 13. Vérifier si Sophie a une fausse identité pour l'escroquerie
?- owns_fake_identity(sophie, escroquerie).

% 14. Vérifier si John a des empreintes sur l'arme pour le vol
?- has_fingerprint_on_weapon(john, vol).

% 15. Vérifier si Mary a des empreintes sur l'arme pour l'assassinat
?- has_fingerprint_on_weapon(mary, assassinat).

% 16. Vérifier si Alice a des transactions bancaires pour l'escroquerie
?- has_bank_transaction(alice, escroquerie).

% 17. Vérifier si Bruno a des transactions bancaires pour l'escroquerie
?- has_bank_transaction(bruno, escroquerie).

% 18. Vérifier si John était près de la scène de crime pour le vol
?- was_near_crime_scene(john, vol).

% 19. Vérifier si Mary était près de la scène de crime pour l'assassinat
?- was_near_crime_scene(mary, assassinat).

% 20. Vérifier si Sophie a une fausse identité
?- owns_fake_identity(sophie, escroquerie).

% REQUETES DE CULPABILITE (21-40)
% ================================

% 21. John est-il coupable de vol ?
?- is_guilty(john, vol).

% 22. Mary est-elle coupable d'assassinat ?
?- is_guilty(mary, assassinat).

% 23. Alice est-elle coupable d'escroquerie ?
?- is_guilty(alice, escroquerie).

% 24. Bruno est-il coupable d'escroquerie ?
?- is_guilty(bruno, escroquerie).

% 25. Sophie est-elle coupable d'escroquerie ?
?- is_guilty(sophie, escroquerie).

% 26. John est-il coupable d'assassinat ?
?- is_guilty(john, assassinat).

% 27. Mary est-elle coupable de vol ?
?- is_guilty(mary, vol).

% 28. Alice est-elle coupable d'assassinat ?
?- is_guilty(alice, assassinat).

% 29. Bruno est-il coupable de vol ?
?- is_guilty(bruno, vol).

% 30. Sophie est-elle coupable d'assassinat ?
?- is_guilty(sophie, assassinat).

% 31. Qui est coupable de vol ?
?- is_guilty(X, vol).

% 32. Qui est coupable d'assassinat ?
?- is_guilty(X, assassinat).

% 33. Qui est coupable d'escroquerie ?
?- is_guilty(X, escroquerie).

% 34. De quoi John est-il coupable ?
?- is_guilty(john, X).

% 35. De quoi Mary est-elle coupable ?
?- is_guilty(mary, X).

% 36. De quoi Alice est-elle coupable ?
?- is_guilty(alice, X).

% 37. De quoi Bruno est-il coupable ?
?- is_guilty(bruno, X).

% 38. De quoi Sophie est-elle coupable ?
?- is_guilty(sophie, X).

% 39. Tous les coupables et leurs crimes
?- is_guilty(X, Y).

% 40. Y a-t-il quelqu'un de coupable ?
?- is_guilty(_, _).

% REQUETES SUR LES MOTIFS (41-55)
% ================================

% 41. Qui a un motif pour le vol ?
?- has_motive(X, vol).

% 42. Qui a un motif pour l'assassinat ?
?- has_motive(X, assassinat).

% 43. Qui a un motif pour l'escroquerie ?
?- has_motive(X, escroquerie).

% 44. Quels motifs John a-t-il ?
?- has_motive(john, X).

% 45. Quels motifs Mary a-t-elle ?
?- has_motive(mary, X).

% 46. Quels motifs Alice a-t-elle ?
?- has_motive(alice, X).

% 47. Tous les motifs existants
?- has_motive(X, Y).

% 48. John et Mary ont-ils des motifs communs ?
?- has_motive(john, X), has_motive(mary, X).

% 49. Qui partage un motif avec John ?
?- has_motive(john, X), has_motive(Y, X), Y \= john.

% 50. Qui partage un motif avec Mary ?
?- has_motive(mary, X), has_motive(Y, X), Y \= mary.

% 51. Combien de personnes ont un motif pour le vol ?
?- findall(X, has_motive(X, vol), L), length(L, N).

% 52. Combien de personnes ont un motif pour l'assassinat ?
?- findall(X, has_motive(X, assassinat), L), length(L, N).

% 53. Qui n'a pas de motif pour le vol ?
?- suspect(X), \+ has_motive(X, vol).

% 54. Qui n'a pas de motif pour l'assassinat ?
?- suspect(X), \+ has_motive(X, assassinat).

% 55. Liste de tous les suspects sans motif pour l'escroquerie
?- findall(X, (suspect(X), \+ has_motive(X, escroquerie)), L).

% REQUETES SUR LA PRESENCE SUR SCENE DE CRIME (56-70)
% ===================================================

% 56. Qui était près de la scène de crime pour le vol ?
?- was_near_crime_scene(X, vol).

% 57. Qui était près de la scène de crime pour l'assassinat ?
?- was_near_crime_scene(X, assassinat).

% 58. Qui était près de la scène de crime pour l'escroquerie ?
?- was_near_crime_scene(X, escroquerie).

% 59. Sur quelles scènes John était-il présent ?
?- was_near_crime_scene(john, X).

% 60. Sur quelles scènes Mary était-elle présente ?
?- was_near_crime_scene(mary, X).

% 61. Toutes les présences sur scènes de crime
?- was_near_crime_scene(X, Y).

% 62. Qui était présent sur plusieurs scènes ?
?- was_near_crime_scene(X, Y1), was_near_crime_scene(X, Y2), Y1 \= Y2.

% 63. John et Mary étaient-ils sur la même scène ?
?- was_near_crime_scene(john, X), was_near_crime_scene(mary, X).

% 64. Qui partage une scène avec John ?
?- was_near_crime_scene(john, X), was_near_crime_scene(Y, X), Y \= john.

% 65. Combien de personnes étaient près de la scène de vol ?
?- findall(X, was_near_crime_scene(X, vol), L), length(L, N).

% 66. Qui n'était pas près de la scène d'assassinat ?
?- suspect(X), \+ was_near_crime_scene(X, assassinat).

% 67. Liste des suspects non présents sur la scène de vol
?- findall(X, (suspect(X), \+ was_near_crime_scene(X, vol)), L).

% 68. Qui était sur toutes les scènes de crime ?
?- suspect(X), was_near_crime_scene(X, vol), was_near_crime_scene(X, assassinat), was_near_crime_scene(X, escroquerie).

% 69. Qui n'était sur aucune scène de crime ?
?- suspect(X), \+ was_near_crime_scene(X, _).

% 70. Scènes où il y avait le plus de suspects
?- findall(X-N, (crime_type(X), findall(Y, was_near_crime_scene(Y, X), L), length(L, N)), Results).

% REQUETES SUR LES EMPREINTES (71-80)
% ===================================

% 71. Qui a des empreintes sur l'arme du vol ?
?- has_fingerprint_on_weapon(X, vol).

% 72. Qui a des empreintes sur l'arme de l'assassinat ?
?- has_fingerprint_on_weapon(X, assassinat).

% 73. Sur quelles armes John a-t-il des empreintes ?
?- has_fingerprint_on_weapon(john, X).

% 74. Sur quelles armes Mary a-t-elle des empreintes ?
?- has_fingerprint_on_weapon(mary, X).

% 75. Toutes les empreintes sur armes
?- has_fingerprint_on_weapon(X, Y).

% 76. Qui a des empreintes sur plusieurs armes ?
?- has_fingerprint_on_weapon(X, Y1), has_fingerprint_on_weapon(X, Y2), Y1 \= Y2.

% 77. Combien de personnes ont des empreintes sur l'arme de vol ?
?- findall(X, has_fingerprint_on_weapon(X, vol), L), length(L, N).

% 78. Qui n'a pas d'empreintes sur l'arme d'assassinat ?
?- suspect(X), \+ has_fingerprint_on_weapon(X, assassinat).

% 79. John et Mary ont-ils des empreintes sur la même arme ?
?- has_fingerprint_on_weapon(john, X), has_fingerprint_on_weapon(mary, X).

% 80. Qui partage des empreintes sur arme avec John ?
?- has_fingerprint_on_weapon(john, X), has_fingerprint_on_weapon(Y, X), Y \= john.

% REQUETES SUR LES TRANSACTIONS BANCAIRES (81-90)
% ===============================================

% 81. Qui a des transactions bancaires liées à l'escroquerie ?
?- has_bank_transaction(X, escroquerie).

% 82. Pour quels crimes Alice a-t-elle des transactions ?
?- has_bank_transaction(alice, X).

% 83. Pour quels crimes Bruno a-t-il des transactions ?
?- has_bank_transaction(bruno, X).

% 84. Toutes les transactions bancaires suspectes
?- has_bank_transaction(X, Y).

% 85. Qui a des transactions pour plusieurs crimes ?
?- has_bank_transaction(X, Y1), has_bank_transaction(X, Y2), Y1 \= Y2.

% 86. Combien de personnes ont des transactions pour l'escroquerie ?
?- findall(X, has_bank_transaction(X, escroquerie), L), length(L, N).

% 87. Alice et Bruno ont-ils des transactions pour le même crime ?
?- has_bank_transaction(alice, X), has_bank_transaction(bruno, X).

% 88. Qui partage des transactions avec Alice ?
?- has_bank_transaction(alice, X), has_bank_transaction(Y, X), Y \= alice.

% 89. Qui n'a pas de transactions bancaires suspectes ?
?- suspect(X), \+ has_bank_transaction(X, _).

% 90. Liste des suspects sans transactions pour l'escroquerie
?- findall(X, (suspect(X), \+ has_bank_transaction(X, escroquerie)), L).

% REQUETES COMPLEXES ET COMBINAISONS (91-100)
% ===========================================

% 91. Qui a à la fois un motif et était présent sur la scène pour le vol ?
?- has_motive(X, vol), was_near_crime_scene(X, vol).

% 92. Qui a à la fois un motif et des empreintes pour l'assassinat ?
?- has_motive(X, assassinat), has_fingerprint_on_weapon(X, assassinat).

% 93. Qui a un motif, était présent et a des empreintes pour l'assassinat ?
?- has_motive(X, assassinat), was_near_crime_scene(X, assassinat), has_fingerprint_on_weapon(X, assassinat).

% 94. Qui a des preuves financières et une fausse identité ?
?- has_bank_transaction(X, escroquerie), owns_fake_identity(X, escroquerie).

% 95. Suspects ayant des preuves pour au moins deux types de crimes différents
?- suspect(X), has_motive(X, Y1), has_motive(X, Y2), Y1 \= Y2.

% 96. Qui est impliqué dans tous les types de crimes ?
?- suspect(X), has_motive(X, vol), has_motive(X, assassinat), has_motive(X, escroquerie).

% 97. Compter le nombre total de preuves contre chaque suspect
?- suspect(X),
   findall(1, has_motive(X, _), M), length(M, Motifs),
   findall(1, was_near_crime_scene(X, _), S), length(S, Scenes),
   findall(1, has_fingerprint_on_weapon(X, _), F), length(F, Empreintes),
   findall(1, has_bank_transaction(X, _), B), length(B, Transactions),
   findall(1, owns_fake_identity(X, _), I), length(I, Identites),
   Total is Motifs + Scenes + Empreintes + Transactions + Identites.

% 98. Trouver les suspects avec le plus de preuves
?- findall(X-N, (suspect(X),
   findall(1, (has_motive(X, _); was_near_crime_scene(X, _); has_fingerprint_on_weapon(X, _);
               has_bank_transaction(X, _); owns_fake_identity(X, _)), L),
   length(L, N)), Results),
   max_member(_-Max, Results),
   member(Suspect-Max, Results).

% 99. Vérifier s'il y a des contradictions dans les preuves
?- suspect(X), is_guilty(X, Y1), is_guilty(X, Y2), Y1 \= Y2.

% 100. Résumé complet : tous les coupables avec leurs crimes et preuves
?- is_guilty(X, Y),
   (has_motive(X, Y) -> Motif = oui; Motif = non),
   (was_near_crime_scene(X, Y) -> Scene = oui; Scene = non),
   (has_fingerprint_on_weapon(X, Y) -> Empreintes = oui; Empreintes = non),
   (has_bank_transaction(X, Y) -> Transactions = oui; Transactions = non),
   (owns_fake_identity(X, Y) -> FausseId = oui; FausseId = non),
   write('Coupable: '), write(X), write(', Crime: '), write(Y),
   write(', Motif: '), write(Motif), write(', Scene: '), write(Scene),
   write(', Empreintes: '), write(Empreintes), write(', Transactions: '), write(Transactions),
   write(', Fausse ID: '), write(FausseId), nl.

% =====================================================
% FIN DES 100 REQUETES
% =====================================================
