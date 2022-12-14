CC = gcc
FLAGS = -g -Wall -Wpedantic -Werror
TARGET = jack.c

all:
	$(CC) $(FLAGS) $(TARGET) -o jack
test: all
	cd test/; ./unittest; cd ..;
