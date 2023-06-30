# Parser
Parser is written in Haskell to take a text file of exercise data and convert it into json

## Usage
The data should be of the form:
```
Exercise reps sets weight failure improved
26/06/23
Push
Bench 10 3 150 y y
Dips 10 3 - n n
etc
```
With each workout separated by a newline character.

Dependencies are installed through cabal so the script can be run with: 
```
cabal run
```

