# Credentials

[![Build Status](https://travis-ci.org/brendanhay/credentials.svg?branch=develop)](https://travis-ci.org/brendanhay/credentials)
[![Hackage Version](https://img.shields.io/hackage/v/credentials.svg)](http://hackage.haskell.org/package/credentials)

* [Description](#description)
* [Setup](#setup)
* [Usage](#usage)
    - [Credential Storage](#credential-storage)
    - [Credential Retrieval](#credential-retrieval)
    - [Credential Management and Auditing](#credential-management-and-auditing)
    - [Credential Revisions](#credential-revisions)
    - [IAM Policies](#iam-policies)
* [Security Notes](#security-notes)
* [Contribute](#contribute)
* [Licence](#licence)


## Description

The `credentials` library and CLI provides a unified interface for managing secure, shared credentials.

It uses Amazon Key Management Service (KMS) for master key management, locally
encrypts and decrypts secrets, which are then stored in any of the supported
storage backends. (Currently DynamoDB.)

The use-case is to avoid storing sensitive information such as passwords and
connection strings in plaintext in places such as source control or on
developers' machines. Instead you can securely administer and distribute
secrets, leveraging Amazon's IAM policies for access control and permissions to
ensure limited read-only permissions from production/deployed hosts.

Please see the [introductory blog post](http://brendanhay.nz/credentials) for more information.


## Setup

1. `stack install credentials-cli`
2. Create a new key in KMS called "credentials". You can do this under Identity
   and Access Management > Encryption Keys in the AWS developer console.
3. Make your AWS access credentials available where [amazonka](https://github.com/brendanhay/amazonka) can read them.
   Typically in the `~/.aws/credentials` file, or as `AWS_ACCESS_KEY_ID` and
   `AWS_SECRET_ACCESS_KEY` environment variables.
4. `credentials setup`


## Usage

A basic example of using the CLI is as follows:

```
$ credentials setup
Setting up dynamo:///credentials in eu-central-1.
Running ...
dynamo://dynamodb.eu-central-1.amazonaws.com:443/credentials:
  status: created
```

```
$ credentials insert --name foo --secret "A magical secret."
Writing new revision of foo to dynamo:///credentials in eu-central-1...
dynamo://dynamodb.eu-central-1.amazonaws.com:443/credentials:
  name: foo
  revision: 82687c4
```

```
$ credentials list
Listing contents of dynamo:///credentials in eu-central-1...
dynamo://dynamodb.eu-central-1.amazonaws.com:443/credentials:
  foo:
    - 82687c4 # latest
```

```
$ credentials select --name foo
Retrieving foo from dynamo:///credentials in eu-central-1...
dynamo://dynamodb.eu-central-1.amazonaws.com:443/credentials:
  name: foo
  revision: 82687c4
  secret: A magical secret.
```

Additional means of formatting the output and logging suitable for use in shell scripts is
available, see the `--help` text for more information.

### Credential Storage

### Credential Retrieval

### Credential Management and Auditing

[Encryption Context](http://docs.aws.amazon.com/kms/latest/developerguide/encrypt-context.html)

### Credential Revisions

### IAM Policies

The complete list of AWS KMS and DynamoDB permissions the various operations will
require are:

* Setup and teardown:
    - `CreateTable`
    - `DestroyTable`
    - `ListTables`
* List credentials and revisions:
    - `Scan`
* Insert a new encrypted credential revision:
    - `GenerateDataKey`
    - `Query`
    - `PutItem`
* Select and decrypt an existing credential revision:
    - `Decrypt`
    - `Query`
* Deletion of a credential revision, or revisions:
    - `Query`
    - `DeleteItem`

It's recommended you allow only the minimal set of permissions as your usecase
requires. For example, the following two IAM policies illustrate the minimum
allowable API operations for read/write, and read only access respectively:

**Read and Write**

```json
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": [
        "kms:GenerateDataKey"
      ],
      "Effect": "Allow",
      "Resource": "arn:aws:kms:us-east-1:AWS_ACCOUNT_ID:alias/credentials"
    },
    {
      "Action": [
        "dynamodb:PutItem",
        "dynamodb:Query"
      ],
      "Effect": "Allow",
      "Resource": "arn:aws:dynamodb:us-east-1:AWS_ACCOUNT_ID:table/credentials"
    }
  ]
}
```

**Read Only**

```
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": [
        "kms:Decrypt"
      ],
      "Effect": "Allow",
      "Resource": "arn:aws:kms:us-east-1:AWS_ACCOUNT_ID:alias/credentials"
    },
    {
      "Action": [
        "dynamodb:Query"
      ],
      "Effect": "Allow",
      "Resource": "arn:aws:dynamodb:us-east-1:AWS_ACCOUNT_ID:table/credentials"
    }
  ]
}
```

Remember to replace the use abovce of `AWS_ACCOUNT_ID` with your own account identifier.

## Security Notes


## Contribute

For any problems, comments, or feedback please create an issue [here on GitHub](https://github.com/brendanhay/credentials/issues).


## Licence

`credentials` is released under the [Apache License Version 2.0](http://www.apache.org/licenses/LICENSE-2.0).
