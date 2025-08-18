% Consigne projet IA MI G5 IG
% Projet basé sur la programmation logique (PROLOG)
% Système d'enquête policière (ou judiciaire)
% Deadline: 24 Août 2025

% Types de crime
crime_type(assassinat).
crime_type(vol).
crime_type(escroquerie).

% Faits - Suspects
suspect(john).
suspect(mary).
suspect(alice).
suspect(bruno).
suspect(sophie).

% Faits - Motifs
has_motive(john, vol).
has_motive(mary, assassinat).
has_motive(alice, escroquerie).
has_motive(bruno, escroquerie).
has_motive(sophie, vol).

% Faits - Présence sur les lieux du crime
was_near_crime_scene(john, vol).
was_near_crime_scene(mary, assassinat).
was_near_crime_scene(alice, vol).
was_near_crime_scene(bruno, vol).

% Faits - Empreintes sur l'arme
has_fingerprint_on_weapon(john, vol).
has_fingerprint_on_weapon(mary, assassinat).
has_fingerprint_on_weapon(alice, escroquerie).

% Faits - Transactions bancaires suspectes
has_bank_transaction(alice, escroquerie).
has_bank_transaction(bruno, escroquerie).
has_bank_transaction(sophie, vol).

% Faits - Fausses identités
owns_fake_identity(sophie, escroquerie).
owns_fake_identity(bruno, escroquerie).

% Faits - Témoignages oculaires
eyewitness_identification(john, vol).
eyewitness_identification(mary, assassinat).
eyewitness_identification(alice, escroquerie).

% Règles de culpabilité

% Règle pour vol
is_guilty(Suspect, vol) :-
    has_motive(Suspect, vol),
    was_near_crime_scene(Suspect, vol),
    (has_fingerprint_on_weapon(Suspect, vol)
    ; eyewitness_identification(Suspect, vol)
    ; has_bank_transaction(Suspect, vol)).

% Règle pour assassinat
is_guilty(Suspect, assassinat) :-
    has_motive(Suspect, assassinat),
    was_near_crime_scene(Suspect, assassinat),
    (has_fingerprint_on_weapon(Suspect, assassinat)
    ; eyewitness_identification(Suspect, assassinat)).

% Règle pour escroquerie
is_guilty(Suspect, escroquerie) :-
    has_motive(Suspect, escroquerie),
    (has_bank_transaction(Suspect, escroquerie)
    ; owns_fake_identity(Suspect, escroquerie)
    ; eyewitness_identification(Suspect, escroquerie)).

% Règle générale - un suspect est innocent s'il n'est pas coupable
is_innocent(Suspect, CrimeType) :-
    suspect(Suspect),
    crime_type(CrimeType),
    \+ is_guilty(Suspect, CrimeType).

% Prédicats utilitaires

% Trouver tous les suspects coupables d'un crime
find_guilty_suspects(CrimeType, Suspects) :-
    findall(Suspect, is_guilty(Suspect, CrimeType), Suspects).

% Trouver tous les crimes dont un suspect est coupable
find_crimes_by_suspect(Suspect, Crimes) :-
    findall(Crime, is_guilty(Suspect, Crime), Crimes).

% Compter le nombre d'indices contre un suspect pour un crime
count_evidence(Suspect, CrimeType, Count) :-
    findall(1, evidence_against(Suspect, CrimeType), Evidence),
    length(Evidence, Count).

% Définir ce qui constitue un indice
evidence_against(Suspect, CrimeType) :-
    has_motive(Suspect, CrimeType).
evidence_against(Suspect, CrimeType) :-
    was_near_crime_scene(Suspect, CrimeType).
evidence_against(Suspect, CrimeType) :-
    has_fingerprint_on_weapon(Suspect, CrimeType).
evidence_against(Suspect, CrimeType) :-
    eyewitness_identification(Suspect, CrimeType).
evidence_against(Suspect, CrimeType) :-
    has_bank_transaction(Suspect, CrimeType).
evidence_against(Suspect, CrimeType) :-
    owns_fake_identity(Suspect, CrimeType).

% Afficher un résumé de l'enquête
show_investigation_summary :-
    write('=== RÉSUMÉ DE L\'ENQUÊTE ==='), nl, nl,
    crime_type(Crime),
    write('Crime: '), write(Crime), nl,
    find_guilty_suspects(Crime, Guilty),
    (Guilty = [] ->
        write('Aucun suspect coupable trouvé.'), nl
    ;
        write('Suspects coupables: '), write(Guilty), nl
    ),
    nl,
    fail.
show_investigation_summary.

% Point d'entrée principal
main :-
    write('Système d\'enquête policière'), nl,
    write('================================'), nl, nl,
    current_input(Input),
    write('Entrez le nom du suspect et le type de crime (ex: john, vol): '),
    read(Input, crime(Suspect, CrimeType)),
    (is_guilty(Suspect, CrimeType) ->
        write('Résultat: COUPABLE'), nl,
        write(Suspect), write(' est coupable de '), write(CrimeType), nl
    ;
        write('Résultat: NON COUPABLE'), nl,
        write(Suspect), write(' n\'est pas coupable de '), write(CrimeType), nl
    ),
    halt.

% ==========================================
% GUIDE COMPLET DES REQUÊTES ET LOGIQUE
% ==========================================

% 1. REQUÊTES DE BASE - VÉRIFICATION DE CULPABILITÉ
% ?- is_guilty(john, vol).          % john est-il coupable de vol ?
% ?- is_guilty(mary, assassinat).   % mary est-elle coupable d'assassinat ?
% ?- is_guilty(alice, escroquerie). % alice est-elle coupable d'escroquerie ?
% ?- is_guilty(bruno, escroquerie). % bruno est-il coupable d'escroquerie ?
% ?- is_guilty(sophie, vol).        % sophie est-elle coupable de vol ?

% 2. REQUÊTES D'INNOCENCE
% ?- is_innocent(john, assassinat). % john est-il innocent d'assassinat ?
% ?- is_innocent(mary, vol).        % mary est-elle innocente de vol ?

% 3. REQUÊTES DE RECHERCHE GLOBALE
% ?- find_guilty_suspects(vol, Suspects).        % Qui est coupable de vol ?
% ?- find_guilty_suspects(assassinat, Suspects). % Qui est coupable d'assassinat ?
% ?- find_guilty_suspects(escroquerie, Suspects).% Qui est coupable d'escroquerie ?

% 4. REQUÊTES PAR SUSPECT
% ?- find_crimes_by_suspect(alice, Crimes).  % De quels crimes alice est-elle coupable ?
% ?- find_crimes_by_suspect(john, Crimes).   % De quels crimes john est-il coupable ?
% ?- find_crimes_by_suspect(mary, Crimes).   % De quels crimes mary est-elle coupable ?

% 5. REQUÊTES SUR LES FAITS/INDICES
% ?- has_motive(X, vol).                     % Qui a un motif pour vol ?
% ?- was_near_crime_scene(X, assassinat).   % Qui était près du lieu d'assassinat ?
% ?- has_fingerprint_on_weapon(X, Y).       % Qui a des empreintes sur quelle arme ?
% ?- has_bank_transaction(X, escroquerie).  % Qui a des transactions suspectes ?
% ?- owns_fake_identity(X, Y).              % Qui possède une fausse identité ?
% ?- eyewitness_identification(X, Y).       % Qui a été identifié par témoin ?

% 6. REQUÊTES D'ANALYSE DES INDICES
% ?- count_evidence(john, vol, Count).       % Combien d'indices contre john pour vol ?
% ?- count_evidence(mary, assassinat, Count).% Combien d'indices contre mary ?
% ?- evidence_against(alice, escroquerie).   % Quels indices contre alice ?

% 7. REQUÊTES COMBINÉES ET LOGIQUES
% ?- suspect(X), is_guilty(X, _).           % Tous les suspects coupables de quelque chose
% ?- crime_type(C), is_guilty(_, C).        % Tous les crimes avec au moins un coupable
% ?- suspect(X), \+ is_guilty(X, _).        % Suspects innocents de tout
% ?- has_motive(X, C), \+ is_guilty(X, C).  % Motif mais pas coupable

% 8. REQUÊTES VARIABLES MULTIPLES
% ?- is_guilty(X, Y).                       % Toutes les combinaisons coupable/crime
% ?- has_motive(X, Y), was_near_crime_scene(X, Y). % Motif ET présence
% ?- evidence_against(X, Y).                % Tous les indices contre tous

% 9. REQUÊTES DE VÉRIFICATION LOGIQUE
% ?- forall(suspect(X), (find_crimes_by_suspect(X, C), write(X-C), nl)).
% ?- forall(crime_type(C), (find_guilty_suspects(C, S), write(C-S), nl)).

% 10. REQUÊTE RÉSUMÉ COMPLET
% ?- show_investigation_summary.            % Affichage complet de l'enquête

% ==========================================
% LOGIQUE DU SYSTÈME
% ==========================================

% RÈGLE VOL:
% Un suspect est coupable de vol SI:
% - Il a un motif pour vol ET
% - Il était près du lieu du crime ET
% - (Il a des empreintes sur l'arme OU témoin l'a identifié OU transaction suspecte)

% RÈGLE ASSASSINAT:
% Un suspect est coupable d'assassinat SI:
% - Il a un motif pour assassinat ET
% - Il était près du lieu du crime ET
% - (Il a des empreintes sur l'arme OU témoin l'a identifié)

% RÈGLE ESCROQUERIE:
% Un suspect est coupable d'escroquerie SI:
% - Il a un motif pour escroquerie ET
% - (Il a des transactions bancaires suspectes OU fausse identité OU témoin)

% TESTS LOGIQUES POUR VALIDATION:
test_all_logic :-
    write('=== TESTS DE VALIDATION LOGIQUE ==='), nl,

    % Test 1: Vérifier john pour vol
    write('Test 1 - John et vol: '),
    (is_guilty(john, vol) -> write('COUPABLE') ; write('INNOCENT')), nl,

    % Test 2: Vérifier mary pour assassinat
    write('Test 2 - Mary et assassinat: '),
    (is_guilty(mary, assassinat) -> write('COUPABLE') ; write('INNOCENT')), nl,

    % Test 3: Vérifier alice pour escroquerie
    write('Test 3 - Alice et escroquerie: '),
    (is_guilty(alice, escroquerie) -> write('COUPABLE') ; write('INNOCENT')), nl,

    % Test 4: Compter tous les coupables
    findall(X-Y, is_guilty(X, Y), AllGuilty),
    write('Tous les coupables: '), write(AllGuilty), nl,

    % Test 5: Compter tous les innocents pour chaque crime
    forall(crime_type(C),
        (findall(S, is_innocent(S, C), Innocents),
         write('Innocents de '), write(C), write(': '), write(Innocents), nl)).

% EXEMPLES D'UTILISATION PRATIQUE:
run_investigation_examples :-
    write('=== EXEMPLES D\'ENQUÊTE ==='), nl,

    % Scénario 1: Enquête sur un vol
    write('SCÉNARIO 1: Enquête sur vol'), nl,
    find_guilty_suspects(vol, VolCoupables),
    write('Coupables de vol: '), write(VolCoupables), nl,

    % Scénario 2: Profil d'un suspect
    write('SCÉNARIO 2: Profil de john'), nl,
    find_crimes_by_suspect(john, JohnCrimes),
    write('Crimes de john: '), write(JohnCrimes), nl,
    count_evidence(john, vol, JohnEvidence),
    write('Indices contre john pour vol: '), write(JohnEvidence), nl,

    % Scénario 3: Analyse comparative
    write('SCÉNARIO 3: Analyse comparative escroquerie'), nl,
    forall(suspect(S),
        (count_evidence(S, escroquerie, Count),
         write(S), write(' a '), write(Count), write(' indices pour escroquerie'), nl)).

% Pour lancer tous les tests:
% ?- test_all_logic.
% ?- run_investigation_examples.
