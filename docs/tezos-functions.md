# Tezos functions

### Tezos.sender \(\): address

### Tezos.source \(\): address

### Tezos.chainId \(\): chain\_id

Return the current chainid

### Tezos.amount \(\): mutez

Return the amount sent by the calling transaction

### Tezos.balance \(\): mutez

Return the balance of the current contract

### Tezos.now \(\): timestamp

Same as Timestamp.now, returns the current timestamp.

### Tezos.self \(\): unit contract

### Tezos.selfAddress \(\): address

Return the address of the current contract

### Tezos.address \('a contract\): address

Return the address of _'a contract_

### Tezos.contract \(address\): 'a contract

Return the typed contract of _address_

### Tezos.setDelegate \(address option\): operation

Set or unset the delegate for the current contract

### Tezos.implicitAccount \(key\_hash\): unit contract

Return the contract of the given key\_hash

### Tezos.transfer \(address, mutez\): operation

Transfer mutez to address

### Tezos.transfer \('a contract, 'a, mutez\): operation

Transfer mutez to contract

### Tezos.createContract \('a code, key\_hash option, mutez\): operation

