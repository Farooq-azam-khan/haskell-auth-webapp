# hauth

* Port and Adapter Architecture - by Alistair Cockburn (2005)
        * allow an application to be drive by users, programs, or automated test and to be developed in isolation from its eventual runtime external dependencies (eg. datbases and queues)
        * Strictly separate internal behaviour from external applications 
                * internal: domain logic
                * external: interact with each other through the use of Port and Adapter
* clear separation of domain logic and delivery mechanism is needed. Eg. web is a delivery mechanism, cli is a delivery mechanism, database, queues as well. 
        * if in the future you want to switch the different technologies (eg. mysql to postgres) you can write another adapter without touching any of the domain logic

## In-Memory Database
* Software Transactional Memory 


## Docker set up 
* `docker run --name hauth-postgres -e POSTGRES_PASSWORD=postgres -d -p 5433:5432 postgres` 
* Docker figure out ip address to connect to for the src/Lib.hs
* `sudo docker run --name hauth-redis -d redis`
```bash
docker inspect \
	-f '{{range.NetworkSettings.Networks}}{{.IPAddress}}{{end}}' \
	<container_name_or_id>
```

### Execute `psql`
- `docker exec -it hauth-postgres bash`
- `psql -U postgres`
- `\l`
- `\c hauth`


### Execute reids
 - `sudo docker exec -it hauth-redis sh`

 * Note: need to drop database for migration and testing otherwise it gets errors that extensions and tables exist

 ### Potential docker Compose 
 - redis
 - postgres
 - haskell code 
 - rabbitmq

## Why RabbitMQ?
- queueing system to run backgroun tasks 
- why not multithread?
        1. could hog memory: 
        external queueing system acts as a buffer so that the taks are consuumed according to the capacity of the processors
        2. task will survive application shut down 
        3. distribute the tasks evenly across many nodes 
- will use RabbitMQ for sending a verification email upon user registration 
- is thread safe 

### Execute RabbitMQ
`docker run -d --hostname my-rabbit --name hauth-rabbitmq rabbitmq`

## Libraries
### aeson 
- aeson is the most popular library for working with JSON 
```Haskell
data Value 
        = Object Object
        | Array Array 
        | String Text 
        | Number Scientific 
        | Bool Bool
        | Null
type Object = HashMap Text Value 
type Array = Vector Value 
```
- `Vector` is a data structure from `vector` library. It is an array structure 
- `Scientific` is type from the `scientific` package. It represents an arbitrary-precision number 
``JSON 
{ "id": 123, "name": "Farooq", "hobbies": ["running", "programming"], "country": null}
```
- the above json becomes
```Haskell
import Data.Aeson 
:{
        object  ["id" .= 123
                , "name" .= "Farooq"
                , "hobbies" .= ["running", "programming"]
                , "country" .= Null
:}
```

