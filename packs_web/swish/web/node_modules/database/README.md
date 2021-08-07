# database

`database` is an npm module for communicating with databases.

## ORMs and ODMs are Evil.

The `database` package is designed unlike other Node.js Object-Relation-Mappers and Object-Document-Mappers.

Most ORMs ( such as [JugglingDB](https://github.com/1602/jugglingdb ) ) create a generic Model Class and API and expect developers to create adapters that conform to this generic Model Class specifcation.

**This is a faultly design.** 

Different databases have different APIs for a reason. Trying to create a unified Model Class for every database in existence does not make sense and creates a myriad of software complexity. This approach more often than not will cause more problems then solutions. Most experienced developers will not even consider using solutions like JugglingDB.

## Develop for the Database, not the Software Mapper

`database` is different from other ORMs in that all it really does is provide an extremely small try / require / configuration pattern around existing database modules. Instead of all database adapters trying to conform to a master Model Class, each `database` adapter is responsible for defining it's own methods and structure.

**Instead of enforcing a generic structure on models for all databases, each database can define it's own model API.**

Our goal is to create a semi-generic API for all databases where some API methods overlap and some are specific to that database's adapter. This way, the developer can use the API best suited for their desired database.

## Why not just require the specific database package on it's own?

There is nothing wrong with this approach. If you readily know which package to use and how to use it, you should require it as a stand-alone module. 

The `database` package at it's core can be considered a curated list of the best database packages available for Node.

Instead of having to search through NPM and Github for packages, you can simply require the `database` module and know that you will have a stable and working API for communicating with your application's database.


## Supported Databases

Currently: None.

We have support for CouchDB in our `resource` module. We use CouchDB a lot, and our CouchDB adapter is full-featured. We'll be adapting Couch. If you want to add another database, please do so!

## Adding new Databases

Adding new databases to the `database` package is easy. In most cases, it will be only take a few lines to require the database module and expose it's configuration methods. 

For example: To add MongoDB support you would install the `mongodb` package, then map it's exported methods to a small `mongodb.js` database adapter. A one-to-one mapping of all methods would work. You could also take this opportunity to custom tweak the `mongo` package API in your wrapper.

# License

MIT
