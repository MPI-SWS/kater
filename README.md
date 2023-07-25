Kater: Automating Weak Memory Model Metatheory and Consistency Checking
=========================================================================

Kater is a tool that can prove metatheoretic properties of weak memory models.
It can also be used to generate consistency checking code for stateless model
checking tools like [GenMC](https://github.com/MPI-SWS/genmc).

This repository mirrors an internal repository and is only updated periodically.
For changes between different versions please refer to the CHANGELOG.

Author: Michalis Kokologiannakis.

**NOTE**: Kater's code is currently unstable and undergoing modifications.
Please report any encountered issues to `michalis AT mpi-sws DOT org`.

* [Getting Kater](#getting-kater)
* [Usage](#usage)
* [Troubleshooting](#troubleshooting)
* [License](#license)
* [Contact](#contact)

<a name="getting-genmc">Getting Kater</a>
-----------------------------------------

### Using Docker

To pull a container containing GenMC from [Docker Hub](https://hub.docker.com)
please issue the following command:

		docker pull genmc/kater

### Building from source

#### Dependencies

To use Kater you need a C++17 compiler, GNU autotools, flex (>= 2.6.4)
and bison (>= 3.7.5) . On a Debian-based installation, the necessary
dependencies are met by installing the following packages:

	autoconf automake g++ flex bison

#### Installing

For a default build issue:

		autoreconf --install
		./configure
		make

This will leave the `kater` executable in the `src` directory.
You can either run it from there (as in the examples below), or issue
`make install`.

<a name="usage">Usage</a>
-------------------------

* To see a list of available options run:

		./src/kater --help

* To run a particular test run:

		./src/kater [options] <file>

<!-- * For more detailed usage examples please refer to the [manual](doc/manual.pdf). -->

<a name="license">License</a>
-----------------------------

Kater is distributed under the GPL, version 3 or (at your option) later.
Please see the COPYING file for details on GPLv3.

<a name="contact">Contact</a>
------------------------

For feedback, questions, and bug reports please send an e-mail to
`michalis AT mpi-sws DOT org`.
