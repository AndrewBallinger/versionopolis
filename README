# Versionopolis
The city of versions?

## Not suitable for any use
This is just a toy project to play with HXT and build something (marginally) useful with Haskell.

## Usage
```
git clone git@github.com:AndrewBallinger/versionopolis
cabal install
versionopolis <configuration.yaml>
```

This will read a yaml configuration and produce different versions of an index.html copying over anything in a static directory.
Right now... The yaml configuration needs to contain a list of objects, where each object consists of a "key" string and then one or more string string pairs. Any html elements with ids equal to the first string will be replaced with the second string.

## Example
index.html :
```
<h1 id="headline">Hello</h1>
<p id="text">world</p>
```

versions.yaml :
```
- key: "Version A"
- key: "Version B"
  headline: "Greetings"
  text: "To You"
```

running :
```
versionopolis versions.yaml
```

Inside the build directory there will be a "Version A.html" and a "Version B.html" where "Version B" has been changed and the static files have been copied over.