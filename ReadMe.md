## Motivation
I wanted to do some fun data sciencey things with my gym data. Haskell is not often used in data science because it's hard but its concision, it's functional parsing libraries and its type system make it a good choice for this project. 

## Python Dependencies
These dependencies are only needed for plotting functionality.
Since we are using matplotlib to plot the data we need to do some pip installations
```bash
pip install matplotlib numpy tk scipy
```
In order to do this you will need pip and python3 installed.

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

Haskell's dependencies are installed through stack, so the script can be run with: 
```bash
stack run
```
As of right now it will print the average number of workouts per week, but more functionality will be added in the future.

