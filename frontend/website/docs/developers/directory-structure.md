# Repository Structure

This document describes the file-structure of [the Coda repository](https://github.com/codaprotocol/coda), and what roles various
files play:

- `dockerfiles/`    
    Contains Docker related scripts - TODO explain this better

- `docs/`    
    Documentation for the code and processes for contributing are here. The documentation website with the walkthrough docs lives in `frontend/website/docs`.
 
- `frontend/`   
    All code related to Coda frontend UIs and products

    - `wallet/`   
        Source code for the Coda wallet

    - `website/`    
        Code for https://codaprotocol.com

        - `docs/`   
            Documentation and instructions on joining the Coda network that live at https://codaprotocol.com/docs 

        - `jobs/`   
            Current job openings at O(1) Labs - [we're hiring](https://codaprotocol.com/jobs.html)!
        - `posts/`    
            Markdown docs for blog posts

        - `src/`    
            Source code for the website

        - `static/`   
            Static files like images, etc.

- `rfcs/`    
    This directory contains all accepted RFCs (or "requests for comments") made according to the [RFC process](CONTRIBUTING.md#RFCs).

- `scripts/`

- `src/`    
    All protocol source code, both application and library code, is in this directory.
  
    - `*.opam`    
        These files are needed for our `dune` build system. There must be one for each
        library in `lib`. When you create a library `lib/foo_lib` with a `dune` file giving
        the library's name as `foo_lib`, you must create a `foo_lib.opam` file.
    
    - `config/`    
        Build time config - these .mlh files define compile time constants and their values.
    
    - `app/`    
        Applications live here.
        
        - `cli/`    
            This is the coda client/daemon. It is what you use to run a staker, a snarker, or a simple client for sending and receiving transactions.
        
        - `website/`    
            Soon to be deprecated directory for the website - most of the code has migrated over to `frontend/website/`
        
        - `reformat/`   
            This program runs `ocamlformat` on most of the files in the source tree, with a few exceptions.
        
        - `logproc/`    
            This utility reads from `stdin` and can filter and pretty print the log messages emitted by the coda daemon.
        
        - `kademlia-haskell/`   
            This is a simple wrapper around a Haskell implementation of the kademlia DHT.

    - `external/`   
        Local copies of external libraries which we've had to make some tweaks to.
    
    - `lib/`    
        Libraries powering coda.
        The libraries here basically fall into two categories.
        1. General purpose data-types and functionality. This includes `snarky`, `fold_lib`, `vrf_lib`, `sgn`, and others.
        2. Application specific functionality, structured as a library. This includes `syncable_ledger`, `staged_ledger`, `transaction_snark`, and others.
    