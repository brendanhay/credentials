# Credentials

* [Description](#description)
* [Usage](#usage)
* [Contribute](#contribute)
* [Licence](#licence)


## Description

This library provides a unified interface for managing secure, shared credentials.

It uses Amazon Key Management Service (KMS) for master-keys and then locally
encrypts and decrypts secrets, which are then stored in any of the supported
storage backends.

The use-case is to avoid storing sensitive information such as passwords and
connection strings in plaintext in places such as source control or on
developers' machines. Instead you can securely administer and distribute
secrets, leveraging Amazon's IAM policies for access control and permissions to
ensure limited read-only permissions from production/deployed hosts.

You can embed this library into projects such as web applications to securely
retrieve sensitive information such as database passwords or private keys on startup.


## Usage

Please see the [introductory blog post](http://brendanhay.nz/credentials) for more information.


## Contribute

For any problems, comments, or feedback please create an issue [here on GitHub](https://github.com/brendanhay/credentials/issues).


## Licence

`credentials-cli` is released under the [Apache License Version 2.0](http://www.apache.org/licenses/LICENSE-2.0).
