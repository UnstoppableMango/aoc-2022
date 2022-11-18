SHELL       := /bin/bash

all::	dotnet

dotnet::
	dotnet build aoc-2022.sln

csharp::
	dotnet build aoc-2022-csharp.sln

fsharp::
	dotnet build aoc-2022-fsharp.sln