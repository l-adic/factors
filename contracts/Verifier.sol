//
// Copyright 2017 Christian Reitwiessner
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//
// 2019 OKIMS
//      ported to solidity 0.6
//      fixed linter warnings
//      added requiere error messages
//
//
// SPDX-License-Identifier: GPL-3.0

pragma solidity ^0.8.0;
library Pairing {
    struct G1Point {
        uint X;
        uint Y;
    }
    // Encoding of field elements is: X[0] * z + X[1]
    struct G2Point {
        uint[2] X;
        uint[2] Y;
    }
    /// @return the generator of G1
    function P1() internal pure returns (G1Point memory) {
        return G1Point(1, 2);
    }
    /// @return the generator of G2
    function P2() internal pure returns (G2Point memory) {
        // Original code point
        return G2Point(
            [11559732032986387107991004021392285783925812861821192530917403151452391805634,
             10857046999023057135944570762232829481370756359578518086990519993285655852781],
            [4082367875863433681332203403145435568316851327593401208105741076214120093531,
             8495653923123431417604973247489272438418190587263600148770280649306958101930]
        );
    }
    /// @return r the negation of p, i.e. p.addition(p.negate()) should be zero.
    function negate(G1Point memory p) internal pure returns (G1Point memory r) {
        // The prime q in the base field F_q for G1
        uint q = 21888242871839275222246405745257275088696311157297823662689037894645226208583;
        if (p.X == 0 && p.Y == 0)
            return G1Point(0, 0);
        return G1Point(p.X, q - (p.Y % q));
    }
    /// @return r the sum of two points of G1
    function addition(G1Point memory p1, G1Point memory p2) internal view returns (G1Point memory r) {
        uint[4] memory input;
        input[0] = p1.X;
        input[1] = p1.Y;
        input[2] = p2.X;
        input[3] = p2.Y;
        bool success;
        // solium-disable-next-line security/no-inline-assembly
        assembly {
            success := staticcall(sub(gas(), 2000), 6, input, 0xc0, r, 0x60)
            // Use "invalid" to make gas estimation work
            switch success case 0 { invalid() }
        }
        require(success,"pairing-add-failed");
    }
    /// @return r the product of a point on G1 and a scalar, i.e.
    /// p == p.scalar_mul(1) and p.addition(p) == p.scalar_mul(2) for all points p.
    function scalar_mul(G1Point memory p, uint s) internal view returns (G1Point memory r) {
        uint[3] memory input;
        input[0] = p.X;
        input[1] = p.Y;
        input[2] = s;
        bool success;
        // solium-disable-next-line security/no-inline-assembly
        assembly {
            success := staticcall(sub(gas(), 2000), 7, input, 0x80, r, 0x60)
            // Use "invalid" to make gas estimation work
            switch success case 0 { invalid() }
        }
        require (success,"pairing-mul-failed");
    }
    /// @return the result of computing the pairing check
    /// e(p1[0], p2[0]) *  .... * e(p1[n], p2[n]) == 1
    /// For example pairing([P1(), P1().negate()], [P2(), P2()]) should
    /// return true.
    function pairing(G1Point[] memory p1, G2Point[] memory p2) internal view returns (bool) {
        require(p1.length == p2.length,"pairing-lengths-failed");
        uint elements = p1.length;
        uint inputSize = elements * 6;
        uint[] memory input = new uint[](inputSize);
        for (uint i = 0; i < elements; i++)
        {
            input[i * 6 + 0] = p1[i].X;
            input[i * 6 + 1] = p1[i].Y;
            input[i * 6 + 2] = p2[i].X[0];
            input[i * 6 + 3] = p2[i].X[1];
            input[i * 6 + 4] = p2[i].Y[0];
            input[i * 6 + 5] = p2[i].Y[1];
        }
        uint[1] memory out;
        bool success;
        // solium-disable-next-line security/no-inline-assembly
        // precompiled bn128 curve pairing as per EIP197 - defined as contract "8"
        assembly {
            success := staticcall(sub(gas(), 2000), 8, add(input, 0x20), mul(inputSize, 0x20), out, 0x20)
        }
        require(success,"pairing-opcode-failed");
        return out[0] != 0;
    }
    /// Convenience method for a pairing check for two pairs.
    function pairingProd2(G1Point memory a1, G2Point memory a2, G1Point memory b1, G2Point memory b2) internal view returns (bool) {
        G1Point[] memory p1 = new G1Point[](2);
        G2Point[] memory p2 = new G2Point[](2);
        p1[0] = a1;
        p1[1] = b1;
        p2[0] = a2;
        p2[1] = b2;
        return pairing(p1, p2);
    }
    /// Convenience method for a pairing check for three pairs.
    function pairingProd3(
            G1Point memory a1, G2Point memory a2,
            G1Point memory b1, G2Point memory b2,
            G1Point memory c1, G2Point memory c2
    ) internal view returns (bool) {
        G1Point[] memory p1 = new G1Point[](3);
        G2Point[] memory p2 = new G2Point[](3);
        p1[0] = a1;
        p1[1] = b1;
        p1[2] = c1;
        p2[0] = a2;
        p2[1] = b2;
        p2[2] = c2;
        return pairing(p1, p2);
    }
    /// Convenience method for a pairing check for four pairs.
    function pairingProd4(
            G1Point memory a1, G2Point memory a2,
            G1Point memory b1, G2Point memory b2,
            G1Point memory c1, G2Point memory c2,
            G1Point memory d1, G2Point memory d2
    ) internal view returns (bool) {
        G1Point[] memory p1 = new G1Point[](4);
        G2Point[] memory p2 = new G2Point[](4);
        p1[0] = a1;
        p1[1] = b1;
        p1[2] = c1;
        p1[3] = d1;
        p2[0] = a2;
        p2[1] = b2;
        p2[2] = c2;
        p2[3] = d2;
        return pairing(p1, p2);
    }
}
contract Verifier {
    using Pairing for *;
    struct VerifyingKey {
        Pairing.G1Point alpha1;
        Pairing.G2Point beta2;
        Pairing.G2Point gamma2;
        Pairing.G2Point delta2;
        Pairing.G1Point[83] IC;
    }
    struct Proof {
        Pairing.G1Point A;
        Pairing.G2Point B;
        Pairing.G1Point C;
    }
    function verifyingKey() internal pure returns (VerifyingKey memory vk) {
        vk.alpha1 = Pairing.G1Point(
            0x12d99fe0cc167d8525f3fe25565afb3ae2a7ca753e9572ec8900ab9711f81a66,
            0x100b2bc45b009f9f58e55045200ec9d2f68019d498833002ac0c657ccbba226c
        );

        vk.beta2 = Pairing.G2Point(
            [0x0016e852fd214c843d2c49e55577fbe9c1b4ea8e15a29977012ec472ee0654bd, 0x0a02cd6a5ae452d0f3136f5c23d8357b7e58f5d1f7f1f63a4e2c8ae826276745],
            [0x2f277ebadb702957c5bcc56cf4bb2129242075ff7fd52d670ef71650e04d99f6, 0x1907fd4d81a0d7a9f9771da742ae36c0bfb4d6e2d119696ce9e657c99960d834]
        );
        vk.gamma2 = Pairing.G2Point(
            [0x10498b303ae4404898be8e529ac11dba71a6769fa0c094126b747a9ea37e83d6, 0x216daeb4b55de7c03592ddee4a8b5881489e4d7e2dab41937859b141901f5e7d],
            [0x19625ef49f446ec869ca9e3d30ba9746a3bad244dd7096119338342e1b090928, 0x2026315d2908f028fd8cd16daa4187a472981a45088a7a80b0262395cb1cde64]
        );
        vk.delta2 = Pairing.G2Point(
            [0x08ca47874619d8f61f5005cdff54b908bc97a402d02b264a59665107ffdf5671, 0x05a1ac22f57f5c9465e7c78ef4811eb4456896781e6f92cb52b63c2a6f65d5f1],
            [0x27c3064ed05cbef1cba0086daa5cb5ef2e74e99e7055b1c8c2020961c92c7918, 0x28a5f352483a69be86f18c1e22f6a610571ddfb682d0a6b62695b2728680f34e]
        );
        
        
        vk.IC[0] = Pairing.G1Point( 
            0x29b46e2837740d1429097deaab9a83b10afe5999abc6e4e033695154eff5d273,
            0x0aeb4e30718901460c7cd73c80f4cfe6fc9bc88a555a72601a4051d0522bd8d0
        );                                      
        
        vk.IC[1] = Pairing.G1Point( 
            0x06866398e186d6d2a7aaf4b90efe304f0512ee68ffd548444ac6593743e54148,
            0x03e0b5d472b80dd72000ca6a2fe80cb0e63c7b158c72d000a398f9728d6b9341
        );                                      
        
        vk.IC[2] = Pairing.G1Point( 
            0x2512ae1bf45ad4c1f68a20d68c4f78287a8ff0c3c00d50054bb617f2128aca94,
            0x1e9d7aa30c3e2e20d1f7040a8be4e3db99d94ce4f11401336345ac9a0c164f44
        );                                      
        
        vk.IC[3] = Pairing.G1Point( 
            0x2994d259fc523fd0efc00676d19a28dbdd1c288d43eb177ba08299aa216a0761,
            0x1c9727ff318de0fbef104f6eb283a2c578809cdafb14ba0381be72958cff8e97
        );                                      
        
        vk.IC[4] = Pairing.G1Point( 
            0x260764a0b0e7215c87a3d79b3acf181255a783c9e3c7555a63c79f0826103501,
            0x1bec6d3c83ea6dae1ac48c94cdf1775d0245eccb1e6ca4b3ed97e655d78a435e
        );                                      
        
        vk.IC[5] = Pairing.G1Point( 
            0x281d66b170dbaad1d3fb3bf41f113d1f08ab291b8d2926777a38bcefc8cb254e,
            0x281d7e53c90e38e9f8b637e31252bdce033e1db6e3713f757a22795c43257ceb
        );                                      
        
        vk.IC[6] = Pairing.G1Point( 
            0x12185cf50641015198e89fb76f058188a75098e7c3b07d51034f64ef9760068e,
            0x0d5fdd6c53e41833eacfbc5b5e483c214b5d35690180a426c6ec6d842ded94cf
        );                                      
        
        vk.IC[7] = Pairing.G1Point( 
            0x158af182d245a9c11ca98ca0cf0d762fa3b74fe750314fa3160d9fefd14e7a24,
            0x028bffc67356c67029a6b474e255fb6ea8e4b0966943e462501160d3ae426aa3
        );                                      
        
        vk.IC[8] = Pairing.G1Point( 
            0x256429dfac6836cb6601958226ae3b815e65a1588709edf436d9926c8105463d,
            0x1d7b9794aadc92599cfddfc7a4ccbd3c732f6ec6eb1490249dade8e8819717e5
        );                                      
        
        vk.IC[9] = Pairing.G1Point( 
            0x0abb2aaa332436b2de58ff30be11e8598f7666e382c20eebb4e54c7b072c90ab,
            0x09fbd9992b5078cf99e63fe2e8827cdf4cb2aab5f33bbe4da200423cb4a9aeb4
        );                                      
        
        vk.IC[10] = Pairing.G1Point( 
            0x0e3bfbb436ec6dd1007b845e5cc794359f638cce072f20a80d4832868c08e5c2,
            0x13be9f30b9f4e875c64ead61ffee2cef6c0ba1088d7987e7c8de5cdfd786dc9a
        );                                      
        
        vk.IC[11] = Pairing.G1Point( 
            0x288ac9d57de2a7b84f5125f8c01bf6e39b3827aedf0f33055eb2e299a790a748,
            0x09bc04c95e5708f31a4bbc95aeca14ab33700a55e5c3c018a060b98378cfd992
        );                                      
        
        vk.IC[12] = Pairing.G1Point( 
            0x2198aa40d29139ddccd800803b1a87f4c96b07f5bc7c7b68754f81e09527849b,
            0x2f1ff85f777fa66a58469cb5688283d78ed20e1c106afc6feb0c1ac1f0e0b3a5
        );                                      
        
        vk.IC[13] = Pairing.G1Point( 
            0x28f4cc1a27b0ae846d4468b5d63d0cedb886d274e75f4b1bc99e4119ef38170f,
            0x1d416ad9fd9ba63d1af966790b4266d322bc33ec9e0e37751fa6b464eb37dade
        );                                      
        
        vk.IC[14] = Pairing.G1Point( 
            0x0f25c1373f62dfd152c8ce1dee3b540df12144ced23b0f90a4a24e533b199036,
            0x108da24de64ae08a29841b348396c0920d0bc257fd40a8d08554c6d9775740f6
        );                                      
        
        vk.IC[15] = Pairing.G1Point( 
            0x0fef218a064d1abc8eb342e2bf8cda9bd3242bba7a21778e454440257a0ae48b,
            0x27cbd2501ec3f3e5d9735d44fef4979df871bbd284bf441ad75d58a66d727e0a
        );                                      
        
        vk.IC[16] = Pairing.G1Point( 
            0x2015eba86c1c2226c475c1f894fac5c6012d16fd5c992bd97e460214a0d5b339,
            0x0376cf06c3c63520c1fd845fcac041d3c7b1d635581f472a316fc5ac2ad0eab4
        );                                      
        
        vk.IC[17] = Pairing.G1Point( 
            0x05bf2d846abd1766b345d91c9c4c1c3291967936e6140628072da60c2c387f3f,
            0x113a724971c3bfe49daf799d89ed6974e1e1b5d1919275f8a8e366d4ef64e637
        );                                      
        
        vk.IC[18] = Pairing.G1Point( 
            0x066acfe3758841c63462f6e848d21bcc70faed6cd58af3b52f5ad3637447b699,
            0x2e8b8fc93e5ab60865ca57c4d6c1aa8f8facfe862e52c78116e7e96ceeef7c1b
        );                                      
        
        vk.IC[19] = Pairing.G1Point( 
            0x160ea499bbda296bc72eb6953af210ff942512a29acafe0d3f44c451fbd14c2f,
            0x1489bcc83892dba4e4949816cc6c2eb1d6f59595cd6904f9e2cf1f2232dc1619
        );                                      
        
        vk.IC[20] = Pairing.G1Point( 
            0x0824eecdc0cbcc5fe8bf346e704a4303ef3ef964e3cfdfc5b422213da7dce770,
            0x26d58120b3ba1fb0ce243b72f5f2360ff4e241e11fbef36e740bd2c2874cf655
        );                                      
        
        vk.IC[21] = Pairing.G1Point( 
            0x2e17aed712281e86001b95ecb7298e0c5886aca40bde28dd5cb54976357d890c,
            0x15641338559c64b983729d86beda58934c6c7218f7fbf67700444e199bed9520
        );                                      
        
        vk.IC[22] = Pairing.G1Point( 
            0x25612d525407800f868bd7b46dea96abcc1d270ca28e919a758bfe02760ee3df,
            0x0f815798c68d4532562fb961056354482dc51e3b1fed27a77db91c429506cb1b
        );                                      
        
        vk.IC[23] = Pairing.G1Point( 
            0x2a95aac981df76157620f10a5c9e13fb78d6137b454fc7cfba7cbadbf0e1ab45,
            0x16d34eaddc1689c698f2d41883f599a682850a65ab1296608f9f56f08988edff
        );                                      
        
        vk.IC[24] = Pairing.G1Point( 
            0x16dabf4f62be1827fae6ed3e6a6c3c894c75674507c4b5f4c4a28b0107aaca5e,
            0x26920294b8fd4649452ebce15c5f31239961bd383c9b1f59e3a60e5e709fd7f6
        );                                      
        
        vk.IC[25] = Pairing.G1Point( 
            0x09f83d5598e18711f3090fc4154fcd866fdd38d5cac380ed78ea9adbb72f1530,
            0x2c1dee38007178f7c9a4b7899fecccab66e6ccbb651c17662a666f8f4d53c95d
        );                                      
        
        vk.IC[26] = Pairing.G1Point( 
            0x0b3225afdb098e0d8662ad3f13e2f30da0cb3e5d79b7c620886abf3f75baaa4d,
            0x0373e32f45e2dd756ad99cd9edb1d9ac8f51c7af20327a70807b21c61cf6a283
        );                                      
        
        vk.IC[27] = Pairing.G1Point( 
            0x0f9c235433a34122f86312506cd96fbb7b536e978d2bbce3f59ca391c43e727a,
            0x069a27f3ed754568810a8cce4f8ca6df1bf64e73d057d15e724b73c6b596f1af
        );                                      
        
        vk.IC[28] = Pairing.G1Point( 
            0x07ff43c1044d5274f5b9ffd01a0c27ed2aa2d37c8845fd25d2cc6565ef9944a2,
            0x2010355a46137df12c097bfe2b58a394ce6ac5f2b2e40a200f617e90e2a53fca
        );                                      
        
        vk.IC[29] = Pairing.G1Point( 
            0x09f4636c4a4f39f0f2788f1c646a3f818ac807f362375169b62c53a1835ab3a8,
            0x23d9580a782e5d16ec16f8fe758e11720cfbb04bea833f18d73f560bbfbc35bc
        );                                      
        
        vk.IC[30] = Pairing.G1Point( 
            0x1c912ad914a5dd9c90ae38d9b7c22b3cc959a879353adf1a68cf2a572c394c37,
            0x169a85062dad836d236185eb86eb8a0d64d7166ab2a530b141aadc9a877191c5
        );                                      
        
        vk.IC[31] = Pairing.G1Point( 
            0x0dfea6aa5b4ce1c499090ac3c352a7ec9c6284d0bf57e89775ab4f194eb01255,
            0x1d348f8f16f6a673b1d5eb238eb29bfc22b3a09b0cf93e3438d168d10fe13d28
        );                                      
        
        vk.IC[32] = Pairing.G1Point( 
            0x15ad33ef0343e189724b8c672c6e9029288c164c4e7aa564bfe3cacd62745b04,
            0x2d5d436e58fee9182904c01f01a6702bea4d2e528e433bd84d2123612d20185e
        );                                      
        
        vk.IC[33] = Pairing.G1Point( 
            0x076fde281196b26f568347c95c601a0338f21165f5a3a46ea205892ebe9bb8bc,
            0x04b14489a7821a17f4ea4179459f230c88f9c8e6b554309daff07e14f40431b6
        );                                      
        
        vk.IC[34] = Pairing.G1Point( 
            0x01181db0493b39d3f5584bc24ffeda7a8ad0555d090f03b380c7c0678e6adb53,
            0x23538d8062f824bde06489bd9542ec40a235f8b4c91bcc14adc229c6d2990e17
        );                                      
        
        vk.IC[35] = Pairing.G1Point( 
            0x28df13a996cd5b3f7a4f06a071d7f0908678bbe1f4cc31a06625de79fdb07afc,
            0x0605d9f65befcaaff54cd557a61647d2fb4aa538023cf89d2f80ae966854db9f
        );                                      
        
        vk.IC[36] = Pairing.G1Point( 
            0x1708f0a9a30977779b00d69b944ca5e64efbbb3ef583bbda2e6bd0f40eb703dd,
            0x1b0af06389803e4966a0eec87050d0d32c4fd448c955a3c9c91df4cbb75ef8db
        );                                      
        
        vk.IC[37] = Pairing.G1Point( 
            0x00dee7f5f96c5839bcdc4f26976b821ee2172f09753b8946f051d41726fb7c01,
            0x06452b817e40b9291c7755124659b94e4a433271e150cd3079fac9b3dcf2ca52
        );                                      
        
        vk.IC[38] = Pairing.G1Point( 
            0x1871f506f6f31d8aff61669b53a996fdd3a7bd07613c7d17e49af9f481e6ae6e,
            0x0ce407b49995d04892f6fd1823c1c4cd505d0e41436fa0a7320bb3e87335f1a5
        );                                      
        
        vk.IC[39] = Pairing.G1Point( 
            0x0f85f78c63f36532eec557e1bc3a797fadf607f83484f9ea6375ca9aee55f976,
            0x28b07aa9f684348bc3b0a6edc38aef0863a4792c031f003e3ec366091ac133a8
        );                                      
        
        vk.IC[40] = Pairing.G1Point( 
            0x2b3a060a1e288ee7bd990d3e65e679f332de9d82909ec186621468bd1d8ba96a,
            0x0d1399e36544ace3df2c745d02126d8b58ac3103cd6269a7054cdd3060e2e7f5
        );                                      
        
        vk.IC[41] = Pairing.G1Point( 
            0x14e22054fd402166c9caf318652d2e0442b1aa0bc0b4b46a63a96884732771f4,
            0x0fca81a7118e4dd1774c7b367f61bc71c904b125e9d042a0efedecaa097f4e5d
        );                                      
        
        vk.IC[42] = Pairing.G1Point( 
            0x0c16db3e95d9f7065cb8e2704c13bbbbb7748e9e3c7975d15c1d64c0afbb78fb,
            0x2139c646e7fdff68279e4b393b1be711842ec49bb696663572a777b024e7e9c2
        );                                      
        
        vk.IC[43] = Pairing.G1Point( 
            0x21ff5aefe4cf64d966b6c373e0a34c02e3a0f2857a3e45e8ac0ca20f01813d84,
            0x0ee4c28848fa1545968e4a2578d29c381febdb3297e7d7900312cb1804208a2e
        );                                      
        
        vk.IC[44] = Pairing.G1Point( 
            0x2b307f66d30a7de21a7828d3be72962074399ffe082ba2b560dccaa17f4fa0b0,
            0x10785882583f8a884f3dcbc7426404d9ce3378ed23997cb35d706ae529232faa
        );                                      
        
        vk.IC[45] = Pairing.G1Point( 
            0x2ac4c3a745a3662c9e3cc46d95dde47106d353f744ae3fe2e4a073099fea484e,
            0x2279a0227362c915479db30802ee20a7759cceaf0b931c8111953cba2fde7202
        );                                      
        
        vk.IC[46] = Pairing.G1Point( 
            0x21c7f8f88c6949b1ef120ed896f6fa36fb5b4429ee370cc70a95e344acf4531a,
            0x2b5fa9fb2ba11ec0aaafdbd7c79c112a0b75888551967ebecfe48c3c9bbb4681
        );                                      
        
        vk.IC[47] = Pairing.G1Point( 
            0x2443965df876e2ad30ef48fdee11b71452a144a1dc21802d154b3550db8fd896,
            0x1ce7385c1b94948f7e40acc4cb33056cd74e828d74a0519b96e215b1345900b5
        );                                      
        
        vk.IC[48] = Pairing.G1Point( 
            0x2bbe8788caac397722fef7f8937b694832055a6183edd8735cdc5f744c71fdeb,
            0x1966bc5776d55ba1b0745c7e1ed43d3c4e50945eb5c6bf97cf8c10bdd2566870
        );                                      
        
        vk.IC[49] = Pairing.G1Point( 
            0x0018493605cd14097e78f18fe01fafc89ee432d36e07eff738c07c3ee372b3b1,
            0x27b24d8b6be554c45b1cd03cbb206f78958c1eba5a14faa54812168c7d35131c
        );                                      
        
        vk.IC[50] = Pairing.G1Point( 
            0x1604a332e471492f65e4a003107847fce0ac00b6d0a9201363908e1977a13395,
            0x18aff7f198e5b6644e2800072cfe46a1a7e1f1f2f810f7a09812212b4c55a3f5
        );                                      
        
        vk.IC[51] = Pairing.G1Point( 
            0x0fd5f71c4509b74c9e56b8798fefabf0c384b9ecabdd5f288d3125f939aaac8b,
            0x1714e3dae520680fe81c05471a5d30280ba8b6876071c17a1255689a8b2057d7
        );                                      
        
        vk.IC[52] = Pairing.G1Point( 
            0x18df523cadd8c19daaa74e615f70bc80bc1611917e2a8d4d06a2eaf1e0856e18,
            0x2e5e638c556d439041905b39db5d12fe78f15347cb5c9128f838ff4ca6b27b22
        );                                      
        
        vk.IC[53] = Pairing.G1Point( 
            0x280be4695b1bce9a913163734162b641f2cd53e4c28bb1ba7cf7c9a373981762,
            0x050f3fe1de4a85c694a760a6ee180e3a6c8597a44681223494a73844547fdf74
        );                                      
        
        vk.IC[54] = Pairing.G1Point( 
            0x0a6505abfd2431441508bdd2cb7202ab1f6bb8c4517c5b1ddb75bcc500b9f39d,
            0x147c77e54ac295539838b5eb01824ddca341ee3b39355f1efc24fbb8cc33f89c
        );                                      
        
        vk.IC[55] = Pairing.G1Point( 
            0x25afb151da348ced3dd4cff8242bc8fe3579cdfa0ca0981997ed807f5673495e,
            0x2d1d0860edac273eadf6db9b33a0d5235e0dc97ded7f07ecd68703907b3818dc
        );                                      
        
        vk.IC[56] = Pairing.G1Point( 
            0x2a81a65a6eed469d7d8780556b9e48e07d1fdf7b06de2c724d80858731b850d6,
            0x2e5c80d666b709518a8e08b464ffd9a4b5ec17f162778d77074b208cf2d4a7f2
        );                                      
        
        vk.IC[57] = Pairing.G1Point( 
            0x2750a291ab0925b24150267825bf092a55d34d8cb3f84dcd8df30ada1cdb69e4,
            0x2ba18c19e55428dac1a2b6d0b43153a62b0678025b177ff0e7686b2ffca673c2
        );                                      
        
        vk.IC[58] = Pairing.G1Point( 
            0x003c01a90b5fa0f80f3ae902cccdf1dce9fc9900d5d7a62c49b65c7dcf10965a,
            0x0e50f00272cce063c7d6e8fb2e6a7e988ccba1d240c9c1d4f98837afc40394ff
        );                                      
        
        vk.IC[59] = Pairing.G1Point( 
            0x17708cdb1f91836b530dc35d9c346db8e6e9236d4357933382fa0c6da597a3c9,
            0x1c54622d67d1500f2f89c5aa567d821e7d507dbe406302a9a2e7f0567fbbd187
        );                                      
        
        vk.IC[60] = Pairing.G1Point( 
            0x14410af820c741679f49748fe5ac052d929c1c3e3a3613f1820e9e246870d6d4,
            0x26fc6a68f9aab4e34699097c320ba51c4e9a102a349fcc8e6f07c86f1aea817d
        );                                      
        
        vk.IC[61] = Pairing.G1Point( 
            0x203a8d28d21010d64122f78a09ff33d916adc198d313bdbb58b40c9321965314,
            0x148cf04b4870a1f4a62d4bedf0aeef2bf8a5843b28a6220381f8ee4d99d6b108
        );                                      
        
        vk.IC[62] = Pairing.G1Point( 
            0x2c79b7ef99b31045acf43233d1311e6b8a6d34770d498a99d00ed5637af72637,
            0x2d431af8d6bdbf2eb640230c0d6ffb44a096a2f650155797519e2912853244c9
        );                                      
        
        vk.IC[63] = Pairing.G1Point( 
            0x0d0e9f6501880681af5b06bd19090e62bf3063bb3d5dd8ed6be6e2fd300c471d,
            0x22608fd21a2dcefa3c919143a98c55afdab5f76577a51b879ca670fd77b8e42e
        );                                      
        
        vk.IC[64] = Pairing.G1Point( 
            0x0a841ff161870780d26994eca7865ef67075cd3621dd3c54d8a9df398b50e436,
            0x0710e164880bcf6613a5268436490141b655860499a792de016d8c53d0907ff2
        );                                      
        
        vk.IC[65] = Pairing.G1Point( 
            0x01d95481f3335be8fa76a401722c67ec52ac7da06bdde274d245f11cec137eac,
            0x240f8e5363d2b6052990fea06831daf65e8ebbf99804c21025349a50aa44b927
        );                                      
        
        vk.IC[66] = Pairing.G1Point( 
            0x0b3e5619767e77907111b2d99dd75f71bd8732ef03ffe45a7e9423cbcc8d2859,
            0x1fcc6d58434dd6dffaade80241d6099fc3c19fb2f2e88fa14056ec20b9f7b71b
        );                                      
        
        vk.IC[67] = Pairing.G1Point( 
            0x0397ae8743580f045738a475a0abe4bb95947ec445165e7d839b88bb2c0dd105,
            0x25e30434a75074e0e7098f52509a45c4e7d8099d649b185bb42e92d4519000b2
        );                                      
        
        vk.IC[68] = Pairing.G1Point( 
            0x1f57f05bdd0867e27cf96fad6c827508d600abf796e65640cf604557678d27ab,
            0x19e81f047e425e7514650b8be9e974e2162184e805e3dac5cd1908665e0d20b8
        );                                      
        
        vk.IC[69] = Pairing.G1Point( 
            0x0207257e4f8c8b47ad07c491e83aa1a8b8b4b44d7c14c54b5325a313608cdcc6,
            0x243ccb9ea99e35fee7bde917fa8e2afad2ddde7ca56acb9fdac5dcf9e5bc195f
        );                                      
        
        vk.IC[70] = Pairing.G1Point( 
            0x040502e5777d91161ac6fc1edc6d9482f1c3ef59c3f207e6ee079cf95c706084,
            0x07e5d904e93ca9a8d2e64385db9ec121f174ca73bf1a7532c53fc8271dfbc8f7
        );                                      
        
        vk.IC[71] = Pairing.G1Point( 
            0x10f79e72a92ff7138a7f748c5b867adeaad27521f90769aff94ef5465bf59867,
            0x29954e5fa25e09f991656f324826a063f68b6f06b25e2ff5a646361fd7dac592
        );                                      
        
        vk.IC[72] = Pairing.G1Point( 
            0x26e93b2f06aa956dd367a1c663bcfc57a5156bc31e9042bd64907c5111f2afb7,
            0x254e31f507e870ce5b1c3d051f7314f4a5a92d55c6d59d9198880080281b06d8
        );                                      
        
        vk.IC[73] = Pairing.G1Point( 
            0x2408ca2b5e64d85ff80d596af14f6c080e00186af07222d6c8b53adceb570f7c,
            0x2f0fbb2888b2245150e93523734e772e818dc2c30bd3d975e001af22366c2061
        );                                      
        
        vk.IC[74] = Pairing.G1Point( 
            0x2019467ff66a3e90918c9d733cc61c8670d0da34dcde1a1f5599ffa5fe644d34,
            0x28097c529bfe441b65fab00a1e3ae602af0657322f9819328148eac932d19176
        );                                      
        
        vk.IC[75] = Pairing.G1Point( 
            0x2903c1a4f446ae97d37565f679fb179d40aa3f9fc21b4a86368b9388ae8b5cd6,
            0x146759b268718f48eb220cd8f018cbbda2708ac1c4ea18b4baffb582c8d87aeb
        );                                      
        
        vk.IC[76] = Pairing.G1Point( 
            0x15e47fbe5555d8c038ce3fc127539fcdc3671b0dea1ce274a39df3f4d79d80aa,
            0x13dd8ec0fecd39ce154ce4ce4d06e4bf1780f0c5c61599d57570c9fbb7c764c0
        );                                      
        
        vk.IC[77] = Pairing.G1Point( 
            0x2362d90fc6722299e4eccd6e607394fea4c7652672f9f0320972a26b2566b98e,
            0x11d7b119bba6265e9aa52ecf694541df45af2704fdb5e809514155f264531f66
        );                                      
        
        vk.IC[78] = Pairing.G1Point( 
            0x300d3e69021c42e8ac88498d85fb79fc7dbc1c34ccc59e802a9e3e4412f8fe9f,
            0x2fff9343e34000dcc417128c0bfb4c35affe4ed5f371b0d2c2265e4a8d0922fb
        );                                      
        
        vk.IC[79] = Pairing.G1Point( 
            0x1e29878c9109d8783add96f053bb12810df4166220018ed739dc4631abe69b4c,
            0x1c6449845fcbc34daacb2a84ae1cd868acd27d6d9fe6158319ba806680d7aad4
        );                                      
        
        vk.IC[80] = Pairing.G1Point( 
            0x0ca9a622307c91d28d4176a45378e9ca286d59a9e1791aed72a2b9b367a85f7a,
            0x21ed90cb9b6a02196db0ed365c4e4fc225105a28db2a6c0ee4c7dd393aec88cd
        );                                      
        
        vk.IC[81] = Pairing.G1Point( 
            0x0ee03d7d80012eb3dc12a4597ad112ea2fbfcecbe7df96f06bd5bcb3afb5b240,
            0x1b3c8267175d420c3d672674297942cfb995e69573429ca91b1fbce50634f7f1
        );                                      
        
        vk.IC[82] = Pairing.G1Point( 
            0x0a08805ccbd610d4445b00b0e021e74babbd580bcb2510d0810538e9da8c8171,
            0x25f223bde57f43c675cc780cb34d361f81b9e4abee57c04a5a6437565a3a1f00
        );                                      
        

    }
    function verify(uint[] memory input, Proof memory proof) internal view returns (uint) {
        uint256 snark_scalar_field = 21888242871839275222246405745257275088548364400416034343698204186575808495617;
        VerifyingKey memory vk = verifyingKey();
        require(input.length + 1 == vk.IC.length,"verifier-bad-input");
        // Compute the linear combination vk_x
        Pairing.G1Point memory vk_x = Pairing.G1Point(0, 0);
        for (uint i = 0; i < input.length; i++) {
            require(input[i] < snark_scalar_field,"verifier-gte-snark-scalar-field");
            vk_x = Pairing.addition(vk_x, Pairing.scalar_mul(vk.IC[i + 1], input[i]));
        }
        vk_x = Pairing.addition(vk_x, vk.IC[0]);
        if (!Pairing.pairingProd4(
            Pairing.negate(proof.A), proof.B,
            vk.alpha1, vk.beta2,
            vk_x, vk.gamma2,
            proof.C, vk.delta2
        )) return 1;
        return 0;
    }
    /// @return r  bool true if proof is valid
    function verifyProof(
            uint[2] memory a,
            uint[2][2] memory b,
            uint[2] memory c,
            uint[82] memory input
        ) public view returns (bool) {
        Proof memory proof;
        proof.A = Pairing.G1Point(a[0], a[1]);
        proof.B = Pairing.G2Point([b[0][0], b[0][1]], [b[1][0], b[1][1]]);
        proof.C = Pairing.G1Point(c[0], c[1]);
        uint[] memory inputValues = new uint[](input.length);
        for(uint i = 0; i < input.length; i++){
            inputValues[i] = input[i];
        }
        if (verify(inputValues, proof) == 0) {
            return true;
        } else {
            return false;
        }
    }
}