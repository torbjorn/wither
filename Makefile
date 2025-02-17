GIT_REF ?= $(shell git rev-parse --verify --short HEAD)
BASEDIR = $(shell git rev-parse --show-toplevel)
PROJECT_NAME ?= $(shell basename `pwd` | tr [:upper:] [:lower:] )
USE_TTY ?= $(shell if [ -t 0 ]; then echo "-it"; fi)
IMAGE_NAME ?= project_$(PROJECT_NAME)_$(GIT_REF)-$(USER)

DOCKER_RUN = docker run --rm $(USE_TTY) \
	-v $(PWD):/project \
	-v $(PWD)/.renv_cache:/renv_cache \
	-w /project \
	$(IMAGE_NAME)

.PHONY: build build-prod shell

build:
	docker build --build-arg USER_UID=$(shell id -u) --build-arg USER_GID=$(shell id -g) $(ARGS) -t $(IMAGE_NAME) .

test: build
	$(DOCKER_RUN) R -e 'source("deps.R")'

shell: build
	$(DOCKER_RUN) bash

R: build
	$(DOCKER_RUN) R

rootless-docker:
	cd .devcontainer && ln -sf devcontainer.rootless.json devcontainer.json

normal-docker:
	cd .devcontainer && ln -sf devcontainer.normal.json devcontainer.json
