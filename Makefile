extra_projects= -aP ../axmpp/gnat

utils:
	gprbuild -p $(extra_projects) -Putils.gpr

clean:
	gprclean $(extra_projects) -Putils.gpr

