CC := arm-linux-gnueabihf-gcc
CFLAGS := -static

BUILD_DIR := build

SRCS := $(wildcard *.s)
TARGETS := $(SRCS:%=$(BUILD_DIR)/%.out)

all: $(TARGETS)

$(BUILD_DIR)/%.s.out: %.s
	$(CC) $(CFLAGS) -o $@ $<

# t: t.s
# 	$(CC) $(CFLAGS) -o $@ $<

