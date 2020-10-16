pragma solidity >=0.4.24 <0.7.0;
pragma experimental ABIEncoderV2;
contract ethna{
    address payable public sender; // The account sending payments.
    address payable public recipient; // The account receiving the payments.
    uint256 public expiration; // Timeout in case the recipient never closes.
    uint[][] internal values; // Values in path
    uint public value; // current balance of contract
    address[][] internal addresses;
    uint public maxProofSize;
    uint public maxPathLength;
    uint id; // id of latest state

    constructor (address payable _recipient, uint256 duration)
    public
    payable {
        sender = msg.sender;
        recipient = _recipient;
        expiration = now + duration;
        maxProofSize = 5;
        maxPathLength = 5;
    }



    /// Recipient can close the channel at any time by presenting a
    /// signed state and amount from the sender. The recipient will be sent that amount,
    /// and the remainder will go back to the sender
    function close(uint256 amount, bytes memory signature) public {
        require(msg.sender == recipient);
        require(isValidSignature(amount, signature));
        recipient.transfer(amount);
        selfdestruct(sender);
    }

    /// Part below is concerned only in case of disagreement between two parties

    function add_state(uint idState, uint amount, bytes memory signature) public {
        require(id<idState);
        require(msg.sender == recipient);
        require(isValidSignature(amount, signature));
        id = idState;
        value = amount;
    }

    /// Recipient sends most recent state signed by sender and valid ongoing
    /// transactions, which are completed but not included in latest state
    function addCompletedTransaction(
        uint idTrans,
        address[] memory _a,
        uint[] memory _v,
        address[] memory _pushPath,
        uint[] memory _pushV,
        bytes memory signature,
        bytes memory signatureShort,
        uint256 deadline,
        uint k ///point in path where is channel
    )
        public payable
    {
        require(idTrans > id);
        require(isValidPushSignature(idTrans, _pushPath, _pushV, signatureShort, _a[_a.length-1], deadline));
        address finalRecipient = _a[maxPathLength-1];
        require(isValidPathSignature(idTrans, _a, _v, finalRecipient, signature));///!!!????
        ///checking whether pushPath is proper:
        for(uint j = 0; j < _pushPath.length; ++j){
            require(_a[j] == _pushPath[j] && _v[j] <= _pushV[j]);
        }
        if (_a[k] == sender && _a[k+1] == recipient) {
            value += _v[k];
            id = idTrans;
        }
    }

    /// recipient can present proof of cheating by Pn at k-th channel on path
    function addCheatingProof(
        uint idTrans, 
        address[] memory _a,
        uint[] memory _v,
        address[] memory _pushPath,
        uint[] memory _pushV,
        bytes[5] memory signatures,
        bytes memory signatureShort,
        uint256 deadline,
        uint8 cheatPoint,
        uint noPaths,
        uint k
        
    )
        public payable
    {
    //address nonce = _a1[0]; ///nonce of transaction
        require(idTrans>id);
        uint vMax;
        uint vPrev;
        require(noPaths <= maxProofSize);
        newTableAddress(_a, noPaths);
        newTableInt(_v, noPaths);
        address finalRecipient = _a[_a.length-1];/// setting finalRecipient
        require(isValidPushSignature(idTrans, _pushPath, _pushV, signatureShort, finalRecipient, deadline));   
        for(uint i = 0; i < noPaths; ++i){
            require(addresses[i][addresses[i].length-1] == finalRecipient);  /// every path should end at the same addresses
            require(isValidPathSignature(idTrans, addresses[i], values[i], finalRecipient, signatures[i]));
        ///checking whether pushPath is proper:
            for(uint j = 0; j < _pushPath.length; ++j){
                require(addresses[i][j] == _pushPath[j] && values[i][j] <= _pushV[j]);
                if(values[i][cheatPoint] > vMax){vMax = values[i][cheatPoint];}
                vPrev+= values[i][cheatPoint+1];
            }
        }   
        if (_a[_pushPath.length - 1] == sender && _a[_pushPath.length] == recipient) {
            if(vMax < vPrev){
                value += _pushV[k];
                id = idTrans;
            }
        }
    }

    function closeDisagreement() public payable{
        require(msg.sender == recipient);
        recipient.transfer(value);
        selfdestruct(sender);
    }


    /// if the timeout is reached without the recipient closing the channel,
    /// then the Ether is released back to the sender.
    function claimTimeout() public {
        require(now >= expiration);
        selfdestruct(sender);
    }


    /// core primitive used to validate signatures
    function isValidSignature(uint256 amount, bytes memory signature)
    internal
    view
    returns (bool)
    {
        bytes32 message = prefixed(keccak256(abi.encodePacked(this, amount)));
        // check that the signature is from the payment sender
        return recoverSigner(message, signature) == sender;
    }

    /// Validation of full path
    function isValidPathSignature(uint _id, address[] memory a, uint[] memory v,
                                  address finalRecipient, bytes memory signature)
    internal    
    view
    returns (bool)
    {
        require(a.length < maxPathLength);
        bytes32 message = prefixed(keccak256(abi.encodePacked(this, _id, a, v)));
        // check that signatures come from the payment sender
        require(a[a.length-1] == finalRecipient);
        return recoverSigner(message, signature) == finalRecipient;
    }

    /// Validation of short path
    function isValidPushSignature(
        uint _id,
        address[] memory a,
        uint[] memory v,
        bytes memory signature,
        address finalRecipient,
        uint256 deadline
    )
        internal
        view
        returns (bool)
    {
        require(deadline < now);
        require(a[a.length-3] == sender);
        bytes32 message = prefixed(keccak256(abi.encodePacked(this, _id, a, v, finalRecipient)));
        // check that the signature is from the payment sender
        return recoverSigner(message, signature) == sender;
    }
    
    //internal functions
    function newTableAddress(address[] memory _a, uint no_paths
        ) internal
     {
        for(uint i = 0; i < no_paths; ++i){
            addresses.push([_a[2*i], _a[2*i+1], _a[2*i+2], _a[2*i+3], _a[2*i+4]
                        ]);
        }
    }
            
    function newTableInt(uint[] memory _v, uint no_paths
        ) internal
    {
        for(uint i = 0; i < no_paths; ++i){
            values.push([_v[2*i], _v[2*i+1],_v[2*i+2], _v[2*i+3],_v[2*i+4]
                        ]);
        }
    }
    
    /// core primitive
    function splitSignature(bytes memory sig)
        internal
        pure
        returns (uint8 v, bytes32 r, bytes32 s)
    {
        require(sig.length == 65);
        assembly {
        // first 32 bytes, after the length prefix
        r := mload(add(sig, 32))
        // second 32 bytes
        s := mload(add(sig, 64))
        // final byte (first byte of the next 32 bytes)
        v := byte(0, mload(add(sig, 96)))
        }
        return (v, r, s);
    }
    
    function recoverSigner(bytes32 message, bytes memory sig)
        internal
        pure
        returns (address)
    {
        (uint8 v, bytes32 r, bytes32 s) = splitSignature(sig);
        return ecrecover(message, v, r, s);
    }
    /// build a prefixed hash to mimic the behavior of eth_sign.
    function prefixed(bytes32 hash) internal pure returns (bytes32) {
        return keccak256(abi.encodePacked("\x19Ethereum Signed Message:\n32", hash));
    }
}   
