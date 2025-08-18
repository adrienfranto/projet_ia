% Fakta
homme(adrien).
femme(Adrienne).
parent(adrien,Adrienne).

% Règles
pere(X,Y) :- homme(X), parent(X,Y).
mere(X,Y) :- femme(X), parent(X,Y).


