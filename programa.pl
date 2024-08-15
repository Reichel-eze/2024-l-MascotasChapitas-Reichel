% Parcial Mascotas Chapitas

% 1)
% a) Modelar la relacion entre personas y mascotas

tiene(martin, adopto(pepa, 2014)).
tiene(martin, adopto(frida, 2015)).
tiene(martin, adopto(kali, 2016)).
tiene(martin, adopto(olivia, 2014)).
tiene(martin, compro(piru, 2010)).

tiene(constanza, regalo(abril, 2006)).
tiene(constanza, adopto(mambo, 2015)).

%agrego esto para el ejercicio 7)
tiene(constanza, compro(eze, 2002)).
tiene(constanza, compro(quiel, 2002)).
tiene(constanza, compro(rei, 2002)).

tiene(hector, adopto(abril, 2015)).
tiene(hector, adopto(mambo, 2015)).
tiene(hector, adopto(buenaventura, 1971)).
tiene(hector, adopto(severino, 2007)).
tiene(hector, adopto(simon, 2016)).
tiene(hector, compro(abril, 2006)).

tiene(silvio, regalo(quinchin, 1990)).

tieneMascota(Persona, Mascota, Anio) :- tiene(Persona, adopto(Mascota, Anio)).
tieneMascota(Persona, Mascota, Anio) :- tiene(Persona, regalo(Mascota, Anio)).
tieneMascota(Persona, Mascota, Anio) :- tiene(Persona, compro(Mascota, Anio)).

% Se tienen los siguientes functores:
% - perro(tamaño)
% - gato(sexo, cantidad de personas que lo acariciaron)
% - tortuga(carácter)

mascota(pepa, perro(mediano)).
mascota(frida, perro(grande)).
mascota(piru, gato(macho,15)).
mascota(kali, gato(macho,3)).
mascota(olivia, gato(hembra,16)).
mascota(mambo, gato(macho,2)).
mascota(abril, gato(hembra,4)).
mascota(buenaventura, tortuga(agresiva)).
mascota(severino, tortuga(agresiva)).
mascota(simon, tortuga(tranquila)).
mascota(quinchin, gato(macho,0)).

% b) Al consultar si Constanza adoptó a mambo en el 2008 debe dar falso.
% Justificar conceptualmente lo que fue necesario hacer para conseguirlo.

% ?- tiene(constanza, adopto(mambo, 2008)). --> false
% Me da falso por el concepto de universo cerrado, es decir, como no se 
% encontro esta consulta individual en la base de conocimientos, entonces
% al no estar se considera como falso!! ("Todo lo que NO esta explicitado/
% declarado en la base de conocimientos se considera falso")

% 2) comprometidos/2 se cumple para dos personas cuando adoptaron
% el mismo año a la misma mascota
% Por ejemplo, Hector y Constanza están comprometidos porque adoptaron a mambo el mismo año.

comprometidos(Persona, OtraPersona) :-
    tiene(Persona, adopto(Mascota, Anio)),
    tiene(OtraPersona, adopto(Mascota, Anio)),
    Persona \= OtraPersona.

% 3) locoDeLosGatos/1 se cumple para una persona cuando tiene 
% sólo gatos, pero más de uno.
% Por ejemplo, Constanza es loco de los gatos.

locoDeLosGatos(Persona) :-
    soloGatos(Persona),
    tieneMasDeUnGato(Persona).

soloGatos(Persona) :-
    tieneMascota(Persona, _, _),
    forall(tieneMascota(Persona, Mascota, _), mascota(Mascota, gato(_,_))).
    
tieneMasDeUnGato(Persona) :-
    tieneMascota(Persona, Mascota1, _),
    tieneMascota(Persona, Mascota2, _),
    mascota(Mascota1, gato(_, _)),
    mascota(Mascota2, gato(_, _)),
    Mascota1 \= Mascota2.

% 4) puedeDormir/1 Se cumple para una persona si no tiene mascotas
% que estén chapita (los perros chicos y las tortugas están chapita, 
% y los gatos machos que fueron acariciados menos de 10 veces están
% chapita). 
% Debe ser inversible.
% En los ejemplos todos tienen mascotas chapita.... Así que nadie 
% puede dormir.

puedeDormir(Persona) :-
    tieneMascota(Persona, _, _),
    not((tieneMascota(Persona, Mascota, _), mascota(Mascota, Animal), estaChapita(Animal))).

puedeDormirFORALL(Persona) :-
    tieneMascota(Persona, _, _),
    forall((tieneMascota(Persona, Mascota, _), mascota(Mascota, Animal)), not(estaChapita(Animal))).

%estaChapita(Mascota) :- mascota(Mascota, perro(chico)).
%estaChapita(Mascota) :- mascota(Mascota, tortuga(_)).

%estaChapita(Mascota) :- 
%    mascota(Mascota, gato(macho, CantidadAcariciadas)),
%    CantidadAcariciadas < 10.

estaChapita(perro(chico)).
estaChapita(tortuga(_)).
estaChapita(gato(macho, CantidadMimos)) :- CantidadMimos < 10.

/* 5)
A veces las personas siguen llevando mascotas a sus casas a pesar de
tener mascotas chapitas. En algunos casos esto genera crisis nerviosas...
a) crisisNerviosa/2 es cierto para una persona y un año cuando, 
el año anterior obtuvo una mascota que está chapita y ya antes 
tenía otra mascota que está chapita. 
Por ejemplo, en el 2008 Hector tuvo una crisis nerviosa 
(ya tenía a buenaventura, que es chapita, y en el 2007 
adoptó a severino... Resultado: en el 2008 tuvo una crisis nerviosa).
b) Decir si es inversible (si se pueden hacer preguntas existenciales) 
y por qué.
*/

/*
crisisNerviosa(Persona, AnioCrisis) :-
    AnioAnterior is AnioCrisis - 1,
    tieneMascota(Persona, Mascota, AnioAnterior),
    estaChapita(Mascota),
    AnioAntes < AnioAnterior,
    tieneMascota(Persona, OtraMascota, AnioAntes),
    estaChapita(OtraMascota),
    Mascota \= OtraMascota.
*/

crisisNerviosa(Persona, AnioCrisis) :-
    tieneMascota(Persona, _, Anio),
    adoptoChapitaAnioAnterior(Persona, Anio, AnioAnterior),
    adoptoChapitaAnioPrevio(Persona, AnioAnterior),
    AnioCrisis is Anio + 1.

adoptoChapitaAnioAnterior(Persona, Anio, AnioAnterior) :-
    AnioAnterior is Anio - 1,
    tieneMascota(Persona, Mascota, AnioAnterior),
    mascota(Mascota, Animal),
    estaChapita(Animal).

adoptoChapitaAnioPrevio(Persona, AnioAnterior) :-
    tieneMascota(Persona, Mascota, AnioPrevio),
    AnioPrevio < AnioAnterior,
    mascota(Mascota, Animal),
    estaChapita(Animal).

% 6) mascotaAlfa/2 Relaciona una persona con el nombre de una mascota, 
% cuando esa mascota domina al resto de las mascotas de esa persona. 
% Se sabe que un gato siempre domina a un perro, que un perro grande
% domina a uno chico, que un gato chapita domina a gatos no chapita, 
% y que una tortuga agresiva domina cualquier cosa. 
% Por ejemplo, kali es mascota alfa de Martín.

mascotaAlfa(Persona, MascotaAlfa) :-
    tieneMascota(Persona, MascotaAlfa, _),
    mascota(MascotaAlfa, AnimalAlfa),
    forall((tieneMascota(Persona, OtraMascota, _), mascota(OtraMascota, Animal),
    MascotaAlfa \= OtraMascota), dominaA(AnimalAlfa, Animal)).

dominaA(gato(_, _), perro(_)).
dominaA(perro(grande), perro(chico)).

dominaA(GatoChapita, GatoNoChapita) :-
    estaChapita(GatoChapita), not(estaChapita(GatoNoChapita)).

dominaA(tortuga(agresiva), _).

% 7) materialista/1 se cumple para una persona cuando no tiene 
% mascotas o compró más de las que adoptó. Hacer que sea inversible. 
% Por ejemplo, si Constanza comprara 3 mascotas sería materialista 
% (porque ya tiene 1 adoptada).

materialista(Persona) :-
    not(tieneMascota(Persona, _, _)).

materialista(Persona) :-
    cantidadCompradas(Persona, Compradas),
    cantidadAdoptadas(Persona, Adoptadas),
    Compradas > Adoptadas.

cantidadCompradas(Persona, Cantidad) :-
    tiene(Persona, _),
    findall(Mascota, tiene(Persona, compro(Mascota, _)), Mascotas),
    length(Mascotas, Cantidad).

cantidadAdoptadas(Persona, Cantidad) :-
    tiene(Persona, _),
    findall(Mascota, tiene(Persona, adopto(Mascota, _)), Mascotas),
    length(Mascotas, Cantidad).
    
    
    
