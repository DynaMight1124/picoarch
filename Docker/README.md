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





