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
    not((tieneMascota(Persona, Mascota, _), estaChapita(Mascota))).

estaChapita(Mascota) :- mascota(Mascota, perro(chico)).
estaChapita(Mascota) :- mascota(Mascota, tortuga(_)).

estaChapita(Mascota) :- 
    mascota(Mascota, gato(macho, CantidadAcariciadas)),
    CantidadAcariciadas < 10.





