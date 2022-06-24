# ft_transcendence


## Starting development environment
```cp .env.development .env``` -- copy environment variables (unix)

```COPY %cd%\.env.development %cd%\.env``` --copy environment variables (windows)

```docker-compose up``` -- start dev env 

```docker exec -it <container name> npm install <package name>```  -- if u want to add dependencies