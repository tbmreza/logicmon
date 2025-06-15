test:
	ledit sml tests/basic.sml

poster:
	# ??: build bin using mlton or otherwise
	ledit sml bin/cli.sml

e2e:
	mlton build
	./dist/posterc examples/Output.al

# grammar/sources.cm
