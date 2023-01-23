### Changelog:

##### v1.0.7

- Update erlpool

##### v1.0.6

- Migrate from lager to logger.
- Update erlpool

##### v1.0.5

- Fix broken build
- Update erl_hash

##### v1.0.4

- Fix the sharding key for EVAL command.

##### v1.0.3

- Fix config parser when we have more than 2 nodes in a shard.

##### v1.0.2

- Fix OTP 24 build.

##### v1.0.1

- Fix OTP 23 build.

##### v1.0.0

- Initial implementation
- Using `murmur2` as hashing algorithm and `Jump Consistent Hash algorithm` for key distribution.
- Tested on Mac OSX, Ubuntu 14.04 LTS, Ubuntu 16.04 LTS
