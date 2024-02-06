make :
	dune build --profile=release
	./_build/default/maze_maker.exe
