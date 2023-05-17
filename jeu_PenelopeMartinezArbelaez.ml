#require "graphics";;
open Graphics;;

(*DEFINITION DES TYPES*)
type dessin =  X | O | Vide 
type carreau = {xmin: int; xmax: int; ymin: int; ymax: int; mutable dessin: dessin}
type etat_jeu = { mutable c1: carreau; mutable c2: carreau; mutable c3: carreau; mutable c4: carreau; mutable c5: carreau; mutable c6: carreau; mutable c7: carreau; mutable c8: carreau; mutable c9: carreau; mutable game_over: bool}

(*CONSTANTES*)
let largeur_fenetre = 300
let hauteur_fenetre = 300 + 100
let largeur_carreau = largeur_fenetre/3
let hauteur_carreau = largeur_fenetre/3

(*FONCTIONS GRAPHIQUES*)
let dessine_O carreau =
  let xmin = carreau.xmin and xmax = carreau.xmax and ymin = carreau.ymin and ymax = carreau.ymax in
  draw_circle ((xmin+xmax)/2) ((ymin+ymax)/2) (hauteur_carreau / 3)
;;

let dessine_X carreau = 
  let xmin = carreau.xmin and xmax = carreau.xmax and ymin = carreau.ymin and ymax = carreau.ymax in
  moveto (xmin + ((xmax-xmin)/6)) (ymin + ((ymax-ymin) / 6));
  lineto (xmax - ((xmax-xmin)/6)) (ymax - ((ymax-ymin) / 6));
  moveto (xmin + ((xmax-xmin)/6 )) (ymax - ((ymax-ymin) / 6));
  lineto (xmax - ((xmax-xmin)/6 )) (ymin + ((ymax-ymin) / 6));
;;

let dessine_grille () =
  let dx = largeur_carreau and dy = hauteur_carreau in
  moveto dx 0;
  lineto dx (3*dy);
  moveto (2*dx) (3*dy);
  lineto (2*dx) 0;
  moveto (3*dx) dy;
  lineto 0 dy;
  moveto 0 (2*dy);
  lineto (3*dx) (2*dy);
;;

let fin_du_jeu i = 
  let dx = largeur_carreau and dy = hauteur_carreau in
  set_color (rgb 214 255 219);
  fill_rect 0 dy (3*dx) dy;
  
  set_color black;
  moveto ((3*dx - 102)/2) (dy + (dy/2) + (((dy/2))-13)/2);
  if i = -1 then 
    draw_string "Personne a gagne"
  else if i mod 2 = 0 then 
    draw_string "Joueur O a gagne"
  else 
    draw_string "Joueur X a gagne";
  
  set_color white;
  fill_rect (3*dx/8) (dy + (dy/8)) (3*dx/4) (dy/4);
  fill_rect ((3*dx/2) + (3*dx/8)) (dy + (dy/8)) (3*dx/4) (dy/4);
  set_color black;
  draw_rect (3*dx/8) (dy + (dy/8)) (3*dx/4) (dy/4); 
  draw_rect ((3*dx/2) + (3*dx/8)) (dy + (dy/8)) (3*dx/4) (dy/4); 
  moveto ((dx/4) + 18) (dy + (dy/4) - 7);
  draw_string "Nouveau jeu";
  moveto ((3*dx/2) + (3*dx/4) - 18)  (dy + (dy/4) - 7);
  draw_string "Sortir";
;;


(*EXCEPTIONS*)
let test etat = 
  if (etat.c1.dessin = etat.c2.dessin && etat.c2.dessin = etat.c3.dessin && etat.c3.dessin <> Vide)
    || (etat.c4.dessin = etat.c5.dessin && etat.c5.dessin = etat.c6.dessin && etat.c6.dessin <> Vide)
    || (etat.c7.dessin = etat.c8.dessin && etat.c8.dessin = etat.c9.dessin && etat.c9.dessin <> Vide)
    || (etat.c1.dessin = etat.c4.dessin && etat.c4.dessin = etat.c7.dessin && etat.c7.dessin <> Vide)
    || (etat.c2.dessin = etat.c5.dessin && etat.c5.dessin = etat.c8.dessin && etat.c8.dessin <> Vide)
    || (etat.c3.dessin = etat.c6.dessin && etat.c6.dessin = etat.c9.dessin && etat.c9.dessin <> Vide)
    || (etat.c1.dessin = etat.c5.dessin && etat.c5.dessin = etat.c9.dessin && etat.c9.dessin <> Vide)
    || (etat.c7.dessin = etat.c5.dessin && etat.c5.dessin = etat.c3.dessin && etat.c3.dessin <> Vide) then 
      (etat.game_over <- true;)
;;


exception Fin_du_tour
let tour etat i =
  let carreaux = [etat.c1; etat.c2; etat.c3; etat.c4; etat.c5; etat.c6; etat.c7; etat.c8; etat.c9] in
  let deja_clique = ref false in
  try
    while not !deja_clique do
      let etat_souris = wait_next_event [Button_down] in
      for k = 0 to (List.length carreaux - 1) do
        let carreau = List.nth carreaux k in
        if etat_souris.mouse_x > carreau.xmin && etat_souris.mouse_x < carreau.xmax && etat_souris.mouse_y > carreau.ymin && etat_souris.mouse_y < carreau.ymax then 
          if carreau.dessin = Vide then
            if i mod 2 = 0 then 
              (dessine_O carreau;
              carreau.dessin <- O )
            else 
              (dessine_X carreau;
              carreau.dessin <- X);
            deja_clique := true
      done;
    done;
  with Fin_du_tour -> ()
;;
         

(*JEU*)
let rec jeu () = 
  let choix_final () = 
    let dx = largeur_carreau and dy = hauteur_carreau in
    let etat_souris = wait_next_event [Button_down] in
    let sortir = {xmin = ((3*dx/2) + (3*dx/8)); xmax = ((3*dx/2) + (3*dx/8) + (3*dy/4)); ymin = (dy + (dy/8)); ymax = (dy + (dy/8) + (dy/4)); dessin = Vide} and rejouer = {xmin = (3*dx/8); xmax = ((3*dx/8) + (3*dy/4)); ymin = (dy + (dy/8)); ymax = (dy + (dy/8) + (dy/4)); dessin = Vide} in
    if etat_souris.mouse_x > sortir.xmin && etat_souris.mouse_x < sortir.xmax && etat_souris.mouse_y > sortir.ymin && etat_souris.mouse_y < sortir.ymax then 
      begin
        close_graph ();
      end
    else if etat_souris.mouse_x > rejouer.xmin && etat_souris.mouse_x < rejouer.xmax && etat_souris.mouse_y > rejouer.ymin && etat_souris.mouse_y < rejouer.ymax then 
      begin
        close_graph ();
       jeu ();
      end
  in

  set_window_title "TIC TAC TOE";
  open_graph "";
  resize_window largeur_fenetre hauteur_fenetre;
  dessine_grille ();

  moveto 117 343;
  draw_string "TIC TAC TOE";

  let etat = 
    {
      c1 = {xmin = 0; xmax = largeur_carreau; ymin = 2*hauteur_carreau; ymax = 3*hauteur_carreau; dessin = Vide} ;
      c2 = {xmin = largeur_carreau; xmax = 2*largeur_carreau; ymin = 2*hauteur_carreau; ymax = 3*hauteur_carreau; dessin = Vide} ;
      c3 = {xmin = 2*largeur_carreau; xmax = 3*largeur_carreau; ymin = 2*hauteur_carreau; ymax = 3*hauteur_carreau; dessin = Vide} ;
      c4 = {xmin = 0; xmax = largeur_carreau; ymin = hauteur_carreau; ymax = 2*hauteur_carreau; dessin = Vide} ; 
      c5 = {xmin = largeur_carreau; xmax = 2*largeur_carreau; ymin = hauteur_carreau; ymax = 2*hauteur_carreau; dessin = Vide} ; 
      c6 = {xmin = 2*largeur_carreau; xmax = 3*largeur_carreau; ymin = hauteur_carreau; ymax = 2*hauteur_carreau; dessin = Vide} ;
      c7 = {xmin = 0; xmax = largeur_carreau; ymin = 0; ymax = hauteur_carreau; dessin = Vide} ;
      c8 = {xmin = largeur_carreau; xmax = 2*largeur_carreau; ymin = 0; ymax = hauteur_carreau; dessin = Vide} ;
      c9 = {xmin = 2*largeur_carreau; xmax = 3*largeur_carreau; ymin = 0; ymax = hauteur_carreau; dessin = Vide} ;
      game_over = false
    }
  in
  for i = 1 to 9 do
    if etat.game_over then
      (fin_du_jeu (i-1);
      choix_final ();)
    else
      (tour etat i;
      test etat;)
  done;
  if etat.game_over then
    begin 
      (fin_du_jeu 9;
      choix_final ();)
    end;
  (fin_du_jeu (-1));
  choix_final ();
;;
