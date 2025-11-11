### Building Docker


Copy the Dockerfile into an empty directory and run the below from that same directory, note the .

```
docker build -t picoarch/builder .
```



### Running Docker

```
docker run --rm -v "C:\PicoArch:/workspace" picoarch/builder
```

This is will build all PicoArch packages

Note -v sets the directory, this may vary depending on your operating system, /workspace is where the files are saved within the Docker and should allow you to view the compiled zip files via the directory set above but if you're not running Windows, you will need to adjust this but leave ":/workspace" as is.


### Change repo/branch

Note the stock Dockerfile is setup to pull from DrUm78's Picoarch and will build stock FunkeyS Picoarch packages. If you want to change this to a different fork and maybe a different branch, you'll need to adjust step 6 in the dockerfile.

For instance: 
```
git clone --recurse-submodules https://github.com/DynaMight1124/picoarch
```
This will pull from my fork, but only the main branch, if you want to pull from a different branch set this: 
```
git clone --recurse-submodules -b 320x240 https://github.com/DynaMight1124/picoarch
```
This will pull from my fork but from a branch called 320x240.




