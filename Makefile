SHELL       := /bin/bash
DAYS        := $(shell ls | grep Day)

.PHONY:: $(DAYS) all clean dotnet csharp fsharp haskell rust java

all::	dotnet haskell rust

clean::
	dotnet clean aoc-2022.sln
	stack clean
	cargo clean
	mvn clean

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

java::
	mvn clean compile

$(DAYS)::
	dotnet run --project $@/fsharp
