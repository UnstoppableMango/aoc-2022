SHELL       := /bin/bash
DAYS        := $(shell ls | grep Day)

.PHONY:: $(DAYS) all dotnet csharp fsharp haskell rust

all::	dotnet haskell rust

dotnet::
	dotnet build aoc-2022.sln

csharp::
	dotnet build aoc-2022-csharp.sln

fsharp::
	dotnet build aoc-2022-fsharp.sln

haskell::
	stack build

rust::
	cargo build

$(DAYS)::
	dotnet run --project $@/fsharp
