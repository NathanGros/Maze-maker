open Raylib
open Random


(*types*)

type direction = |Up |Down |Left |Right;;

type graph = {
	width: int;
	height: int;
	adj: direction list array
}
;;

type state = int list list;;


let m = 60;; (*maze width*)
let n = 50;; (*maze height*)
let made_maze = ref false;;
let animate = ref false;;
let bg_color = Color.create 54 55 60 255;;
let tile_color = Color.create 91 92 99 255;;
let wall_color = Color.create 54 55 60 255;;


(*functions*)

let window_init () =
	init_window 0 0 "Maze maker";
	clear_background bg_color;
	set_target_fps 600
;;


let draw_maze g =
	clear_background bg_color;
	let w = get_screen_width () in
	let h = get_screen_height () in
	let px = min ((w-200) / g.width) ((h-200) / g.height) in
	let start_x = h / 2 - px * g.height / 2 in
	let start_y = w / 2 - px * g.width / 2 in
	draw_rectangle start_y start_x (g.width * px) (g.height * px) tile_color;
	for i = 0 to Array.length g.adj - 1 do
		let l = g.adj.(i) in
		if not (List.mem Up l) then draw_rectangle (start_y + (i mod g.width) * px) (start_x + (i / g.width) * px - 1) px 3 wall_color;
		if not (List.mem Down l) then draw_rectangle (start_y + (i mod g.width) * px) (start_x + (i / g.width + 1) * px - 1) px 3 wall_color;
		if not (List.mem Left l) then draw_rectangle (start_y + (i mod g.width) * px - 1) (start_x + (i / g.width) * px) 3 px wall_color;
		if not (List.mem Right l) then draw_rectangle (start_y + (i mod g.width + 1) * px - 1) (start_x + (i / g.width) * px) 3 px wall_color;
	done;
	begin_drawing ();
	end_drawing ()
;;


let graph_init x y =
	{width = x; height = y; adj = Array.make (x*y) []}
;;


let graph_of_state s =
	let g = graph_init m n in
	for i = 0 to List.length s - 1 do
		let l = List.nth s i in
		for j = 0 to List.length l - 1 do
			let c = List.nth l j in
			if List.mem (c+1) l then g.adj.(c) <- Right :: g.adj.(c);
			if List.mem (c-1) l then g.adj.(c) <- Left :: g.adj.(c);
			if List.mem (c+m) l then g.adj.(c) <- Down :: g.adj.(c);
			if List.mem (c-m) l then g.adj.(c) <- Up :: g.adj.(c);
		done
	done;
	g
;;


let rec print_list l start =
	if start then print_string "[";
	match l with
	|[] -> print_string "]\n"
	|t::[] -> print_int t; print_string "]"
	|t::q -> print_int t; print_string "; "; print_list q false
;;


let rec print_state s start =
	if start then print_string "[";
	match s with
	|[] -> print_string "]\n"
	|t::[] -> print_list t true; print_string "]\n"
	|t::q -> print_list t true; print_string "; "; print_state q false
;;


let rec join s a b l =
	match s, l with
	|[], [] -> []
	|t::q, [] -> if List.mem a t || List.mem b t then join q a b t else t::join q a b l
	|t::q, l1 -> if List.mem a t || List.mem b t then (l1@t) :: q else t::join q a b l
	|_, _ -> failwith "error"
;;


let rec is_connected s a b =
	match s with
	|[] -> false
	|t::q -> if List.mem a t && List.mem b t then true else is_connected q a b
;;


let knuth_shuffle l =
	let t = Array.of_list l in
	for i = 0 to Array.length t - 1 do
		let n = Random.int (i+1) in
		let a = t.(i) in
		t.(i) <- t.(n);
		t.(n) <- a
	done;
	Array.to_list t
;;


let pick_class n =
	let nb = Random.int n in
	let l = knuth_shuffle (List.init n (fun i -> i)) in
	List.init (nb+1) (fun i -> List.nth l i)
;;


let make_maze x y =
	Random.self_init ();
	let g = graph_init x y in
	let s = ref (List.init (x*y) (fun i -> [i])) in
	(*make maze on y-1 lines*)
	for i = 0 to y - 2 do
		(*join on line i*)
		let l = knuth_shuffle (List.init (x-1) (fun j -> i*x + j)) in
		for j = 0 to x - 2 do
			let c = List.nth l j in
			if (not (is_connected !s c (c+1))) && (Random.int 2 = 0) then begin
				g.adj.(c) <- Right :: g.adj.(c);
				g.adj.(c+1) <- Left :: g.adj.(c+1);
				if !animate then draw_maze g;
				s := join !s c (c+1) []
			end
		done;
		(*join line i and line i+1*)
		let n = ref 1 in
		for j = 1 to x - 1 do
			if is_connected !s (i*x + j - 1) (i*x + j) then
				n := !n + 1
			else begin
				let next_l = pick_class !n in
				List.iter (fun c -> 
									g.adj.(i*x + j-1 - c) <- Down :: g.adj.(i*x + j-1 - c);
									g.adj.((i+1)*x + j-1 - c) <- Up :: g.adj.((i+1)*x + j-1 - c);
									if !animate then draw_maze g;
									s := join !s (i*x + j-1 - c) ((i+1)*x + j-1 - c) []
							 ) next_l
				;
				n := 1
			end
		done;
		let next_l = pick_class !n in
		List.iter (fun c -> 
							g.adj.(i*x + x-1 - c) <- Down :: g.adj.(i*x + x-1 - c);
							g.adj.((i+1)*x + x-1 - c) <- Up :: g.adj.((i+1)*x + x-1 - c);
							if !animate then draw_maze g;
							s := join !s (i*x + x-1 - c) ((i+1)*x + x-1 - c) []
					 ) next_l
		;
	done;
	(*connect last line*)
	let l = knuth_shuffle (List.init (x-1) (fun j -> (y-1)*x + j)) in
	for j = 0 to x - 2 do
		let c = List.nth l j in
		if (not (is_connected !s c (c+1))) then begin
			g.adj.(c) <- Right :: g.adj.(c);
			g.adj.(c+1) <- Left :: g.adj.(c+1);
	draw_maze g;
			s := join !s c (c+1) []
		end
	done;
	
	draw_maze g;

	made_maze := true
;;


(*main*)

let rec loop () =
	match window_should_close () with
	| true -> close_window ()
	| false ->
		if not !made_maze then make_maze m n;
		if is_key_pressed A then animate := not !animate;
		if is_key_pressed R then made_maze := false;
		begin_drawing ();
		end_drawing ();
		loop ()
;;

let _ =
	window_init ();
	loop ()
;;
