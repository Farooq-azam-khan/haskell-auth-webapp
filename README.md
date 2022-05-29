# hauth

* Port and Adapter Architecture - by Alistair Cockburn (2005)
        * allow an application to be drive by users, programs, or automated test and to be developed in isolation from its eventual runtime external dependencies (eg. datbases and queues)
        * Strictly separate internal behaviour from external applications 
                * internal: domain logic
                * external: interact with each other through the use of Port and Adapter
* clear separation of domain logic and delivery mechanism is needed. Eg. web is a delivery mechanism, cli is a delivery mechanism, database, queues as well. 
        * if in the future you want to switch the different technologies (eg. mysql to postgres) you can write another adapter without touching any of the domain logic
