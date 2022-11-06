#==============================================================
#
# SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF SAMPLE CODE LICENSE AGREEMENT,
# http://software.intel.com/en-us/articles/intel-sample-source-code-license-agreement/
#
# Copyright Intel Corporation
#
# THIS FILE IS PROVIDED "AS IS" WITH NO WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT
# NOT LIMITED TO ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
# PURPOSE, NON-INFRINGEMENT OF INTELLECTUAL PROPERTY RIGHTS.
#
# =============================================================

CC := icc
SRCDIR := src
BUILDDIR := out
CFLAGS := -O2 -std=c99 -wd3180
LIBFLAGS :=
RUNTIME_FLAGS := 
TARGET := $(BUILDDIR)/openmp_samples

SOURCES := $(wildcard $(SRCDIR)/*.c)
OBJECTS := $(patsubst $(SRCDIR)/%,$(BUILDDIR)/%,$(SOURCES:.c=.o))

release: CFLAGS += -qopenmp
release: LIBFLAGS += -qopenmp
release: clean $(TARGET)

serial: clean $(TARGET)

$(TARGET): $(OBJECTS)
	$(CC) $^ $(LIBFLAGS) -o $(TARGET)

$(BUILDDIR)/%.o: $(SRCDIR)/%.c
	@mkdir -p $(BUILDDIR)
	$(CC) -c $(CFLAGS) -o $@ $<

run: $(TARGET)
ifeq ($(shell uname -s),Darwin)
	@export  DYLD_LIBRARY_PATH="$(LIBRARY_PATH)";ulimit -s 64000; ./$(TARGET) $(RUNTIME_FLAGS) 
else	
	@ulimit -s 64000; ./$(TARGET) $(RUNTIME_FLAGS)
endif

clean:
	@echo "Cleaning..."
	@rm -fr $(BUILDDIR) $(TARGET) 2>/dev/null || true

.PHONY: clean
