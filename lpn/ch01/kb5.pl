loves(vincent,mia).
loves(marsellus,mia).
loves(pumpkin,honey_bunny).
loves(honey_bunny,pumpkin).

/**
 * original definition:
 * jealous(X,Y):- loves(X,Z), loves(Y,Z).
 */

jealous(X,Y):- loves(X,Z), loves(Y,Z), X \= Y.
