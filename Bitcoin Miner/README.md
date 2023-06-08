# Distributed Operating System Principles - Project 1 - Bitcoing Mining
*Date: 24th September, 2022.*

**Team Members:**
- Simran Sunil Kukreja (UFID: 72070369, s.kukreja@ufl.edu)<br>
- Rachana Nitinkumar Gugale (UFID: 63532454, rgugale@ufl.edu)

**Implementation Details:**

![Architecture Diagram](/ArchitectureDiagram.png)

Bitcoin mining has been performed using a server and multiple clients. The number of leading 0s are taken as input by the server and passed on to the boss actor of the client. The boss actors of the client and server then spawn multiple actors on the client and server respectively to distribute the mining workload and achieve parallelism. Each actor then hashes the input string (GatorId + ';' + randomString) using SHA 256, and generates a bitcoin. The leading 0s of this bitcoin are then compared with the specified number of 0s provided as input, and if they match, they are sent to the boss actor of the server and printed in the output.

**Code execution steps:**<br>
**-Server:**<br>
    1. Navigate to the project directory. [cd directoryName]<br>
    2. Set the cookie on the server using : erl -name@IPv4ofServer -setcookie x, 
    &nbsp;&nbsp;&nbsp;&nbsp;'x' is the cookie name to be set.<br>
    1. Start the server:<br>
    &nbsp;&nbsp;&nbsp;&nbsp;driver:startServer(4) [4 is the number of zeros expected in the mined bitcoins]

**-Client:**<br>
    1. Navigate to the project directory. [cd directoryName]<br>
    2. Set the cookie on the client using : erl -name name@IPv4ofClient -setcookie x,<br>
    &nbsp;&nbsp;&nbsp;&nbsp;'x' is the cookie name to be set.<br>
    3. Start the client:<br>
    &nbsp;&nbsp;&nbsp;&nbsp;driver:startClient(name@IPv4ofServer)

**Result of running the program for input=4:**
```
(node1@10.20.225.31)2> driver:startServer(4).
(node1@10.20.225.31)3>  [Server: <0.112.0>]      s.kukreja;ycloozdhf     0000bbd82b0683d6042e13bc51d6e3b073f6a8ea123893fdb6a2b91b30b259b2
(node1@10.20.225.31)3>  [Server: <0.139.0>]      s.kukreja;afwdyld4i     000048a23920fb240ec2e51d8e8ca162fb8b8288bfdc3efb5cb33c719f2e7200
(node1@10.20.225.31)3>  [Server: <0.162.0>]      s.kukreja;en3uqttma     00008c7f449b6816c494f03caa81d5e1a00db12f55d7236820477d3bedfb1290
(node1@10.20.225.31)3>  [Server: <0.203.0>]      s.kukreja;ouu7lxnun     00006517ca26ed8f03fcfeedb9c8bf57720ae3824743a1450e79a3aef42ca23f
(node1@10.20.225.31)3>  [Server: <0.118.0>]      s.kukreja;xz/ckzuyb     000013bfb8774cc8f70b7f851d31bb5a4169b79170e70771d258ea7026823737
(node1@10.20.225.31)3>  [Server: <0.132.0>]      s.kukreja;o/09qxs8i     00009925c2f8d150cb664d0dcb90df60e5a3de09006006c441addb1c8a8936c3
(node1@10.20.225.31)3>  [Server: <0.122.0>]      s.kukreja;r5l/b9n1h     0000fd9f21e64c2b163a5bff42b72336b888111118248b7da2fc2c28b338a335
(node1@10.20.225.31)3>  [Server: <0.116.0>]      s.kukreja;lgpt50kgy     00003714ca046cd878e8cf0451a54b6bc7a2ac553f769937111a230e064144cc
(node1@10.20.225.31)3>  [Server: <0.93.0>]       s.kukreja;1dz0/65tk     000081a7a8ee641380dc51d31c42dd9d93c6abefdddba858e4dc86feb579cf21
(node1@10.20.225.31)3>  [Server: <0.131.0>]      s.kukreja;yfanfdb3e     00005bae9d1cbf7201796c7bdc0d76d94e6e47067b02e25ef7adff24cf39e3c6
(node1@10.20.225.31)3>  [Server: <0.102.0>]      s.kukreja;6msg0i4it     000089ee281270ee3f3a84cf745ea98a00b665f7c90fc10cdf7159823868bcd7
(node1@10.20.225.31)3>  [Server: <0.176.0>]      s.kukreja;i4rlxwtcg     0000cb0d47098604899b7c39c81b4a7dc8cea7ef6b017ea3f9243ddd74e8de0a
(node1@10.20.225.31)3>  [Server: <0.111.0>]      s.kukreja;9tw7iflht     0000538cd1dd3edcc4382a3b403fc39603995b8a33657760d9607540a72dccc8
(node1@10.20.225.31)3>  [Server: <0.196.0>]      s.kukreja;2dveg20dg     00002a59de5ec9371179daf55da2f92ea980b344c71771c0aa50bf9501751f27
(node1@10.20.225.31)3>  [Server: <0.115.0>]      s.kukreja;v14ykhfpi     0000741d64b1571a450a73864f704fe1a566b66642b8b337ebec3ffa934732c4
(node1@10.20.225.31)3>  [Server: <0.144.0>]      s.kukreja;t+rpvkr+h     0000f6efde98ff46bd4de2c9fcb7b64c34daf3d9b0440d19e98ddbffadc659b5
(node1@10.20.225.31)3>  [Server: <0.110.0>]      s.kukreja;ds4jbxggu     00000da6f9b68f9ed4827f6924526b655ce725979ef4dd0f795529caf8a17db4
(node1@10.20.225.31)3>  [Server: <0.109.0>]      s.kukreja;fhr1g/pf9     0000bca4a27da4a7ea2963cde28cb91ff58f158c24c3fb3b700877d463780c99
(node1@10.20.225.31)3>  [Server: <0.197.0>]      s.kukreja;5b8pptvdp     0000d6ca4d6e2223d34538cee40a40124d9805b8bddd45fbb2da5b39f61d1cd6
(node1@10.20.225.31)3>  [Server: <0.153.0>]      s.kukreja;g5fmekrbq     00007b1a3d5a8c05585952c054c075361d8e59aa9b67e7e497b21cf8f09765cd
(node1@10.20.225.31)3>  [Server: <0.172.0>]      s.kukreja;972xnkopp     0000bf58fd685959f66676ad0476c46a408635f452db9d647b9417f9920d3700
(node1@10.20.225.31)3>  [Server: <0.142.0>]      s.kukreja;pgdccfn68     0000b4081c9823f74ffbea198cc5592d78c5d04e4a40e7fcc59ad233449476b9
(node1@10.20.225.31)3>  [Server: <0.152.0>]      s.kukreja;nt63j3w2r     0000be208ed066e1d0c7e82107585b91bdc4e8d5a611a93b994d0ff5dc238e16
(node1@10.20.225.31)3>  [Server: <0.104.0>]      s.kukreja;ai8seo1/v     00004c971580e888c822274b4c982cf2ae10fb597bf1a9a3dea0e23f82e26252
(node1@10.20.225.31)3>  [Server: <0.150.0>]      s.kukreja;hatpxra1f     000033ba7ae08eb2a8fb3b9d09f8aedc364fc0568a4660f73d9114c298c8a95a
(node1@10.20.225.31)3>  [Server: <0.160.0>]      s.kukreja;87b+comoe     00008c53008dad7adb998f875d5808d10a2fd7116ce5c3c3f7b593b31e53173d
(node1@10.20.225.31)3>  [Server: <0.126.0>]      s.kukreja;xlpdimx7s     00008681b6e9bad81034fbe5b3340c8a0fed0e30b1c45e3396d91ad560e1fb41
(node1@10.20.225.31)3>  [Server: <0.98.0>]       s.kukreja;swo+s/vkj     000054dfd01b85dd42ca41227a6a258e58f76481f656fcdfa34f05707c7f8d22
(node1@10.20.225.31)3>  [Server: <0.195.0>]      s.kukreja;iciw3zail     00007f873dfaa7ac5147f3c98717db16b4e7900d7c94ac50ec65e993aa2bb598
(node1@10.20.225.31)3>  [Server: <0.206.0>]      s.kukreja;v7fohokdn     0000ae7605a1cf34fcf09881d26e22eeaf385466c3a5848524cbc21f6f42eeb3
(node1@10.20.225.31)3>  [Server: <0.114.0>]      s.kukreja;xwn61ybxv     000060c79457e91d726146576fb42e03fc04e33dfc30c753ccdc7ac6ef290385
(node1@10.20.225.31)3>  [Server: <0.151.0>]      s.kukreja;px3976knl     0000eb163bbca091c4acfe4ad7780b5c88da64fa70d8f32dd1eb272315384d9d
(node1@10.20.225.31)3>  [Server: <0.159.0>]      s.kukreja;ulgauukfw     000020efb82a445e131b21e54cefc147a9d0b32d517b7f02356d09cf96b86f8a
(node1@10.20.225.31)3>  [Server: <0.191.0>]      s.kukreja;wdp25ovaa     00007545ef78a83de7a392c599a0a1c7833576b6929d727f63fe77afe76e1b25
(node1@10.20.225.31)3>  [Server: <0.94.0>]       s.kukreja;ua+dlzbh+     00007a26982ab4cdb4982ca0adff1544e84743581fa8b5ece4d006491ca20250
(node1@10.20.225.31)3>  [Server: <0.103.0>]      s.kukreja;hbbk00dte     0000b2df96699640534a1b2a0c7432d17257910ee5b4c2389e23a6746ecfe0ca
(node1@10.20.225.31)3>  [Server: <0.125.0>]      s.kukreja;bsbhtfsgf     0000ef9ac741f3b510f0ca723df9138ea7c079cb7de4f88267f9d42b3e067976
(node1@10.20.225.31)3>  [Server: <0.127.0>]      s.kukreja;co6ziimvf     0000980ba96b48387dda29998d9f1073d9cd8bbb5a3a5fab3f502d42f1da557f
(node1@10.20.225.31)3>  [Server: <0.141.0>]      s.kukreja;iijyht+g/     0000010d585f2f8962906ceee1697cdc75d469622d15fc28126a81e98c923b03
(node1@10.20.225.31)3>  [Server: <0.205.0>]      s.kukreja;1mrf1ixwo     00009c3b7c5c5323727e81720d6eda851ef0e37dff371c71fab0241c787244b0
(node1@10.20.225.31)3>  [Server: <0.134.0>]      s.kukreja;esmfzxi0f     0000033b86ed41605e438c054a299007661584e26203013b7e7138d71bf706e2
(node1@10.20.225.31)3>  [Server: <0.157.0>]      s.kukreja;jsoskkcdz     0000f45a638e88ec8a2b5d021711995d0cc66f3978dd341853e3e9d4a84fb4a7
(node1@10.20.225.31)3>  [Server: <0.181.0>]      s.kukreja;ei7qv9yv6     0000fdfd383ab83bd563f0b53048865ec645ee124e8bc93442b57bc55707ac90
(node1@10.20.225.31)3>  [Server: <0.146.0>]      s.kukreja;l/y1utyom     0000451f863257a050d0c319d805c8319ef0529bb5b3861601535cc2233b2e7f
(node1@10.20.225.31)3>  [Server: <0.158.0>]      s.kukreja;xwm/bk8je     0000c2a57d181beb3e61f921a1a4093c0e2b839836a222cf241e2af4c4c01476
(node1@10.20.225.31)3>  [Server: <0.147.0>]      s.kukreja;yu7icgidl     000033f5c693fd8a659c80135af8aff23b53cfd7ea321c579d2a820e2fe1f3ff
(node1@10.20.225.31)3>  [Server: <0.96.0>]       s.kukreja;fuc/iessh     0000d69a91ccd4efb2a017a3c7a5d0054ac07f7bc2c910d2ae6b756defe93288
(node1@10.20.225.31)3>  [Server: <0.106.0>]      s.kukreja;5jutffacq     00007faab3d7c8a1f0486f7f41fea60259efa2c11bd4fd7fdf5a017a39b0301e
(node1@10.20.225.31)3>  [Server: <0.211.0>]      s.kukreja;hchmlv9az     00006e67b2996fb99d728e1e55f4fabae6e544bf461eee9dada16ecc67981fe4
(node1@10.20.225.31)3>  [Server: <0.95.0>]       s.kukreja;4e8ckurmr     0000ccf181015b0fc5c6269e31ee8be69dceadd7d6c7377fc1f79aac385ba823
(node1@10.20.225.31)3>  [Server: <0.182.0>]      s.kukreja;abjjw6c69     000086b17fdf1176a8f3b0a208f71ee6df9d72ff9de1c9138a67b9c295b6b97d
(node1@10.20.225.31)3>  [Server: <0.137.0>]      s.kukreja;ce9lzhi49     0000eb2f6a95d4d332c7e57ea87e81fde2c3e72eae70b9df991b12c05be1678d
(node1@10.20.225.31)3>  [Client:<13818.87.0>]    s.kukreja;ftb2navoc     0000e5c061a0408a03e1fe9529afe756145b94d7540c3a0dc41315efeec0b745
(node1@10.20.225.31)3>  [Server: <0.108.0>]      s.kukreja;b0ry7gw3n     00005ed7481f369bb58c6fa7f3eb0f6c45f5a6c564064b80c8abdecbae5f54d4
(node1@10.20.225.31)3>  [Server: <0.155.0>]      s.kukreja;3gtccn0xz     000011c18a1cab481b0299935cb504387dc3438dd26d127a63775d9fd33c766b
(node1@10.20.225.31)3>  [Client:<13818.87.0>]    s.kukreja;ihw597a90     00006c0b7a11bbc09a5255f230d6b001f936b571a2d0e86ce9591227cf4c90f6
(node1@10.20.225.31)3>  [Client:<13818.87.0>]    s.kukreja;rxfohjow5     0000a302272a941b4e39c50972e0db0294a13735ae88b814abe188733c38babc
(node1@10.20.225.31)3>  [Client:<13818.87.0>]    s.kukreja;zegwoeee7     0000f45f9f7be2426b894654b913ac320cd18745ce8fcb85aa7d705b34a8f1c4
(node1@10.20.225.31)3>  [Client:<13818.87.0>]    s.kukreja;pegywdguy     00001fccea245ec4fcf60c40179925376fc5e1dfda896f82400c6d829685af92
(node1@10.20.225.31)3>  [Client:<13818.87.0>]    s.kukreja;abv7ww1jz     0000c62f5ba146b480f64825927268efd99d04918c6d076cc049688598761cac
(node1@10.20.225.31)3>  [Client:<13818.87.0>]    s.kukreja;vf/w4ipdd     000020839feab2407a7c353c41a4df1d83f414d395dc8e95f1a598247fcc7981
(node1@10.20.225.31)3>  [Server: <0.168.0>]      s.kukreja;tqqv6ma0g     000011ccf47a42d19dbfc27a8a81b44ed2669a3afd5c0f841718a917e0ed28af
(node1@10.20.225.31)3>  [Client:<13818.87.0>]    s.kukreja;g/yzqhtts     0000e8fd49df026d12da1634c5d014ce7b0ece971494b8d1841508ec5be3c758
(node1@10.20.225.31)3>  [Server: <0.117.0>]      s.kukreja;jpwufxbeg     00005203f60e267d1dc729b7df1a01d99a54a4f03eb87352add2416d52deed45
(node1@10.20.225.31)3>  [Client:<13818.87.0>]    s.kukreja;q/1/cjskf     00003413b3bde33f50313a9e92cce51b93691bfd84454f8e73f4a17bfa28a560
(node1@10.20.225.31)3>  [Server: <0.154.0>]      s.kukreja;k+icksaoh     0000588c78fa4ae9e6da5fcf0b2c65da182107ffa85053833a389c91fd09324b
(node1@10.20.225.31)3>  [Server: <0.149.0>]      s.kukreja;hs0fz7abh     00008777a5b609e6bc6791c406b1c95bbdedb94920e0393a01de5ddd7ae4b736
(node1@10.20.225.31)3>  [Client:<13818.87.0>]    s.kukreja;jxirwwaxd     0000d316d94c75ddf98b583f62e30b4de16cf7674065681ee40027b7ed7e26fa
(node1@10.20.225.31)3>  [Server: <0.198.0>]      s.kukreja;s6beqhac8     000043b78df0fb72a2a90e9e4b166c1d9163407e0400719d4753290b0201b284
(node1@10.20.225.31)3>  [Client:<13818.87.0>]    s.kukreja;htr2nj0za     0000beafaf7a05485c5361dd679b4064e54a2ddde9d7431b8d8d6fc4e487f01d
(node1@10.20.225.31)3>  [Client:<13818.87.0>]    s.kukreja;fzvlyuypt     000009bbd9cec2a885ae65c311fb4f26cdb1765243c00bce4e083e6a74035f93
(node1@10.20.225.31)3>  [Client:<13818.87.0>]    s.kukreja;qgyxhgqvt     00000e7582e11cc20178a29d8503a9e0d34998005ffd66b4578af8e9bf66fc47
(node1@10.20.225.31)3>  [Client:<13818.87.0>]    s.kukreja;orjh800dy     00004bbd8cab36414b1abffaf602b24a1f1ed90aeae86cd40b2521228549b4f6
(node1@10.20.225.31)3>  [Client:<13818.87.0>]    s.kukreja;krwikkxik     0000d4907021fce285339377c59b1040b1b597cf4c6459c959ee2a853fd57e7f
(node1@10.20.225.31)3>  [Client:<13818.87.0>]    s.kukreja;6awtxjhlu     000003f30d6b063fde62d0d6a9116b82d9d2a7455e66b6ec3162ea3ca753b609
(node1@10.20.225.31)3>  [Server: <0.100.0>]      s.kukreja;au0mqa7/c     000073f2fbce60abe921c2b75f88c286345b9884931823180e56bc145d06a28c
(node1@10.20.225.31)3>  [Client:<13818.87.0>]    s.kukreja;klc7caeub     00001e65b38631811af136dc8fd6617eb34413987f88740d90f2e2d0eb133b92
(node1@10.20.225.31)3>  [Client:<13818.87.0>]    s.kukreja;rwxxsp90h     00009fa59bc903ef4b406aadb013381fc96a31f360716301de415a6649c58bc3
(node1@10.20.225.31)3>  [Server: <0.133.0>]      s.kukreja;35biesalm     00002e0e5ed72496a160f19142b1c619fc73dbf3a555f35ded6eac2aa2ff8ef8
(node1@10.20.225.31)3>  [Client:<13818.87.0>]    s.kukreja;bsdzmbhtk     00001cd2cb24c04b6cc502ea30b06a6af5de98348d201df57effafdab13493d4
(node1@10.20.225.31)3>  [Client:<13818.87.0>]    s.kukreja;bq8p/fot5     0000381afa7b1573b671aa56b393a098f9c3d05e858a291724c467576401286f
 Runtime: 26703         Clock time: 3955        Ratio: 6.751706700379267
 ```

**Size of the work unit that resulted in the best performance for this implementation:**

Explanation:<br>
Our best determined work unit value is 1000000, as highlighted in the table below. We reached this work unit through experimenting with different values as shown below to reach the optimal performance measure in a distributed client-server system.

With 240 actors spawned collectively on the client and service machines, and a work unit of 1000000 assigned per actor, we were able to achieve optimal parallelism and perform bitcoing mining at a ratio of 6.7352 of CPU time to Clock time.

Number of Actors spawned: 240

| WorkUnit      | Ratio of CPU time to Clock time  |
|---------------|----------------------------------|
| 100000        | 6.237                            |
| **1000000**   | **6.752**                        |
| 10000000      | 6.506                            |
| 100000000     | 6.336                            |
| 1000000000    | 6.199                            |
| 10000000000   | 6.170                            |

**Result for the coin with the most 0s:**<br>




**The largest number of working machines we ran the code with:**<br>
    We were able to perform bitcoin mining using 2 machines, with 3 Clients and 1 Server, through work distribution between the machines and actors.
