##
## EPITECH PROJECT, 2024
## IMAGE COMPRESSOR
## File description:
## Makefile
##

NAM = glados

all:
	stack build --copy-bins --local-bin-path . && \
	mv ./glados-exe ./$(NAM)

clean:
	stack clean

fclean: clean
	rm -f $(NAM)

re: fclean all

.PHONY: all clean fclean re