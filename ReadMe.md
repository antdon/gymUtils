## Motivation
I wanted to do some fun data sciencey things with my gym data. Haskell is not often used in data science because it's hard but its concision, it's functional parsing libraries and its type system make it a good choice for this project. 
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

Dependencies are installed through stack so the script can be run with: 
```
stack run
```
As of right now it will print the average number of workouts per week but more functionality will be added in the future.

