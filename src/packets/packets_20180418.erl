%% all packet sizes are measured in bytes (8 bits)
%% except 0 which are not defined here but in the packet

-module(packets_20180418).

-export([packet_size/1]).

packet_size(16#083c) -> 19;
packet_size(16#0064) -> 55;
packet_size(16#0065) -> 17;
packet_size(16#0066) -> 3;
packet_size(16#0067) -> 37;
packet_size(16#0068) -> 46;
packet_size(16#0069) -> 0;
packet_size(16#006A) -> 23;
packet_size(16#006B) -> 0;
packet_size(16#006C) -> 3;
packet_size(16#006D) -> 157;
packet_size(16#006E) -> 3;
packet_size(16#006F) -> 2;
packet_size(16#0070) -> 3;
packet_size(16#0071) -> 28;
packet_size(16#0072) -> 22;
packet_size(16#0073) -> 11;
packet_size(16#0074) -> 3;
packet_size(16#0075) -> 0;
packet_size(16#0076) -> 9;
packet_size(16#0077) -> 5;
packet_size(16#0078) -> 55;
packet_size(16#0079) -> 53;
packet_size(16#007A) -> 58;
packet_size(16#007B) -> 60;
packet_size(16#007C) -> 42;
packet_size(16#007D) -> 2;
packet_size(16#007E) -> 105;
packet_size(16#007F) -> 6;
packet_size(16#0080) -> 7;
packet_size(16#0081) -> 3;
packet_size(16#0082) -> 2;
packet_size(16#0083) -> 2;
packet_size(16#0084) -> 2;
packet_size(16#0085) -> 10;
packet_size(16#0086) -> 16;
packet_size(16#0087) -> 12;
packet_size(16#0088) -> 10;
packet_size(16#0089) -> 11;
packet_size(16#008A) -> 29;
packet_size(16#008B) -> 23;
packet_size(16#008C) -> 14;
packet_size(16#008D) -> 0;
packet_size(16#008E) -> 0;
packet_size(16#0090) -> 7;
packet_size(16#0091) -> 22;
packet_size(16#0092) -> 28;
packet_size(16#0093) -> 2;
packet_size(16#0094) -> 19;
packet_size(16#0095) -> 30;
packet_size(16#0096) -> 0;
packet_size(16#0097) -> 0;
packet_size(16#0098) -> 3;
packet_size(16#0099) -> 0;
packet_size(16#009A) -> 0;
packet_size(16#009B) -> 34;
packet_size(16#009C) -> 9;
packet_size(16#009D) -> 17;
packet_size(16#009E) -> 17;
packet_size(16#009F) -> 20;
packet_size(16#00A0) -> 23;
packet_size(16#00A1) -> 6;
packet_size(16#00A2) -> 14;
packet_size(16#00A3) -> 0;
packet_size(16#00A4) -> 0;
packet_size(16#00A5) -> 0;
packet_size(16#00A6) -> 0;
packet_size(16#00A7) -> 9;
packet_size(16#00A8) -> 7;
packet_size(16#00A9) -> 6;
packet_size(16#00AA) -> 7;
packet_size(16#00AB) -> 4;
packet_size(16#00AC) -> 7;
packet_size(16#00AE) -> 0;
packet_size(16#00AF) -> 6;
packet_size(16#00B0) -> 8;
packet_size(16#00B1) -> 8;
packet_size(16#00B2) -> 3;
packet_size(16#00B3) -> 3;
packet_size(16#00B4) -> 0;
packet_size(16#00B5) -> 6;
packet_size(16#00B6) -> 6;
packet_size(16#00B7) -> 0;
packet_size(16#00B8) -> 7;
packet_size(16#00B9) -> 6;
packet_size(16#00BA) -> 2;
packet_size(16#00BB) -> 5;
packet_size(16#00BC) -> 6;
packet_size(16#00BD) -> 44;
packet_size(16#00BE) -> 5;
packet_size(16#00BF) -> 3;
packet_size(16#00C0) -> 7;
packet_size(16#00C1) -> 2;
packet_size(16#00C2) -> 6;
packet_size(16#00C3) -> 8;
packet_size(16#00C4) -> 6;
packet_size(16#00C5) -> 7;
packet_size(16#00C6) -> 0;
packet_size(16#00C7) -> 0;
packet_size(16#00C8) -> 0;
packet_size(16#00C9) -> 0;
packet_size(16#00CA) -> 3;
packet_size(16#00CB) -> 3;
packet_size(16#00CC) -> 6;
packet_size(16#00CD) -> 3;
packet_size(16#00CE) -> 2;
packet_size(16#00CF) -> 27;
packet_size(16#00D0) -> 3;
packet_size(16#00D1) -> 4;
packet_size(16#00D2) -> 4;
packet_size(16#00D3) -> 2;
packet_size(16#00D4) -> 0;
packet_size(16#00D5) -> 0;
packet_size(16#00D6) -> 3;
packet_size(16#00D7) -> 0;
packet_size(16#00D8) -> 6;
packet_size(16#00D9) -> 14;
packet_size(16#00DA) -> 3;
packet_size(16#00DB) -> 0;
packet_size(16#00DC) -> 28;
packet_size(16#00DD) -> 29;
packet_size(16#00DE) -> 0;
packet_size(16#00DF) -> 0;
packet_size(16#00E0) -> 30;
packet_size(16#00E1) -> 30;
packet_size(16#00E2) -> 26;
packet_size(16#00E3) -> 2;
packet_size(16#00E4) -> 6;
packet_size(16#00E5) -> 26;
packet_size(16#00E6) -> 3;
packet_size(16#00E7) -> 3;
packet_size(16#00E8) -> 8;
packet_size(16#00E9) -> 19;
packet_size(16#00EA) -> 5;
packet_size(16#00EB) -> 2;
packet_size(16#00EC) -> 3;
packet_size(16#00ED) -> 2;
packet_size(16#00EE) -> 2;
packet_size(16#00EF) -> 2;
packet_size(16#00F0) -> 3;
packet_size(16#00F1) -> 2;
packet_size(16#00F2) -> 6;
packet_size(16#00F3) -> 0;
packet_size(16#00F4) -> 21;
packet_size(16#00F5) -> 11;
packet_size(16#00F6) -> 8;
packet_size(16#00F7) -> 17;
packet_size(16#00F8) -> 2;
packet_size(16#00F9) -> 26;
packet_size(16#00FA) -> 3;
packet_size(16#00FB) -> 0;
packet_size(16#00FC) -> 6;
packet_size(16#00FD) -> 27;
packet_size(16#00FE) -> 30;
packet_size(16#00FF) -> 10;
packet_size(16#0100) -> 2;
packet_size(16#0101) -> 6;
packet_size(16#0102) -> 6;
packet_size(16#0103) -> 30;
packet_size(16#0104) -> 79;
packet_size(16#0105) -> 31;
packet_size(16#0106) -> 10;
packet_size(16#0107) -> 10;
packet_size(16#0108) -> 0;
packet_size(16#0109) -> 0;
packet_size(16#010A) -> 4;
packet_size(16#010B) -> 6;
packet_size(16#010C) -> 6;
packet_size(16#010D) -> 2;
packet_size(16#010E) -> 11;
packet_size(16#010F) -> 0;
packet_size(16#0110) -> 10;
packet_size(16#0111) -> 39;
packet_size(16#0112) -> 4;
packet_size(16#0113) -> 25;
packet_size(16#0114) -> 31;
packet_size(16#0115) -> 35;
packet_size(16#0116) -> 17;
packet_size(16#0117) -> 18;
packet_size(16#0118) -> 2;
packet_size(16#0119) -> 13;
packet_size(16#011A) -> 15;
packet_size(16#011B) -> 20;
packet_size(16#011C) -> 68;
packet_size(16#011D) -> 2;
packet_size(16#011E) -> 3;
packet_size(16#011F) -> 16;
packet_size(16#0120) -> 6;
packet_size(16#0121) -> 14;
packet_size(16#0122) -> 0;
packet_size(16#0123) -> 0;
packet_size(16#0124) -> 21;
packet_size(16#0125) -> 8;
packet_size(16#0126) -> 8;
packet_size(16#0127) -> 8;
packet_size(16#0128) -> 8;
packet_size(16#0129) -> 8;
packet_size(16#012A) -> 2;
packet_size(16#012B) -> 2;
packet_size(16#012C) -> 3;
packet_size(16#012D) -> 4;
packet_size(16#012E) -> 2;
packet_size(16#012F) -> 0;
packet_size(16#0130) -> 6;
packet_size(16#0131) -> 86;
packet_size(16#0132) -> 6;
packet_size(16#0133) -> 0;
packet_size(16#0134) -> 0;
packet_size(16#0135) -> 7;
packet_size(16#0136) -> 0;
packet_size(16#0137) -> 6;
packet_size(16#0138) -> 3;
packet_size(16#0139) -> 16;
packet_size(16#013A) -> 4;
packet_size(16#013B) -> 4;
packet_size(16#013C) -> 4;
packet_size(16#013D) -> 6;
packet_size(16#013E) -> 24;
packet_size(16#013F) -> 26;
packet_size(16#0140) -> 22;
packet_size(16#0141) -> 14;
packet_size(16#0142) -> 6;
packet_size(16#0143) -> 10;
packet_size(16#0144) -> 23;
packet_size(16#0145) -> 19;
packet_size(16#0146) -> 6;
packet_size(16#0147) -> 39;
packet_size(16#0148) -> 8;
packet_size(16#0149) -> 9;
packet_size(16#014A) -> 6;
packet_size(16#014B) -> 27;
packet_size(16#014C) -> 0;
packet_size(16#014D) -> 2;
packet_size(16#014E) -> 6;
packet_size(16#014F) -> 6;
packet_size(16#0150) -> 110;
packet_size(16#0151) -> 6;
packet_size(16#0152) -> 0;
packet_size(16#0153) -> 0;
packet_size(16#0154) -> 0;
packet_size(16#0155) -> 0;
packet_size(16#0156) -> 0;
packet_size(16#0157) -> 6;
packet_size(16#0158) -> 0;
packet_size(16#0159) -> 54;
packet_size(16#015A) -> 66;
packet_size(16#015B) -> 54;
packet_size(16#015C) -> 90;
packet_size(16#015D) -> 42;
packet_size(16#015E) -> 6;
packet_size(16#015F) -> 42;
packet_size(16#0160) -> 0;
packet_size(16#0161) -> 0;
packet_size(16#0162) -> 0;
packet_size(16#0163) -> 0;
packet_size(16#0164) -> 0;
packet_size(16#0165) -> 30;
packet_size(16#0166) -> 0;
packet_size(16#0167) -> 3;
packet_size(16#0168) -> 14;
packet_size(16#0169) -> 3;
packet_size(16#016A) -> 43;
packet_size(16#016C) -> 43;
packet_size(16#016D) -> 14;
packet_size(16#016E) -> 186;
packet_size(16#016F) -> 182;
packet_size(16#0170) -> 14;
packet_size(16#0171) -> 30;
packet_size(16#0172) -> 10;
packet_size(16#0173) -> 3;
packet_size(16#0174) -> 0;
packet_size(16#0175) -> 6;
packet_size(16#0176) -> 106;
packet_size(16#0177) -> 0;
packet_size(16#0178) -> 4;
packet_size(16#0179) -> 5;
packet_size(16#017A) -> 4;
packet_size(16#017B) -> 0;
packet_size(16#017C) -> 6;
packet_size(16#017D) -> 7;
packet_size(16#017E) -> 0;
packet_size(16#017F) -> 0;
packet_size(16#0180) -> 6;
packet_size(16#0181) -> 3;
packet_size(16#0182) -> 106;
packet_size(16#0183) -> 10;
packet_size(16#0184) -> 10;
packet_size(16#0185) -> 34;
packet_size(16#0187) -> 6;
packet_size(16#0188) -> 8;
packet_size(16#0189) -> 4;
packet_size(16#018A) -> 4;
packet_size(16#018B) -> 4;
packet_size(16#018C) -> 29;
packet_size(16#018D) -> 0;
packet_size(16#018E) -> 10;
packet_size(16#018F) -> 6;
packet_size(16#0190) -> 23;
packet_size(16#0191) -> 86;
packet_size(16#0192) -> 24;
packet_size(16#0193) -> 2;
packet_size(16#0194) -> 30;
packet_size(16#0195) -> 102;
packet_size(16#0196) -> 9;
packet_size(16#0197) -> 4;
packet_size(16#0198) -> 8;
packet_size(16#0199) -> 4;
packet_size(16#019A) -> 14;
packet_size(16#019B) -> 10;
packet_size(16#019C) -> 0;
packet_size(16#019D) -> 6;
packet_size(16#019E) -> 2;
packet_size(16#019F) -> 6;
packet_size(16#01A0) -> 3;
packet_size(16#01A1) -> 3;
packet_size(16#01A2) -> 37;
packet_size(16#01A3) -> 5;
packet_size(16#01A4) -> 11;
packet_size(16#01A5) -> 26;
packet_size(16#01A6) -> 0;
packet_size(16#01A7) -> 4;
packet_size(16#01A8) -> 4;
packet_size(16#01A9) -> 6;
packet_size(16#01AA) -> 10;
packet_size(16#01AB) -> 12;
packet_size(16#01AC) -> 6;
packet_size(16#01AD) -> 0;
packet_size(16#01AE) -> 4;
packet_size(16#01AF) -> 4;
packet_size(16#01B0) -> 11;
packet_size(16#01B1) -> 7;
packet_size(16#01B2) -> 0;
packet_size(16#01B3) -> 67;
packet_size(16#01B4) -> 12;
packet_size(16#01B5) -> 18;
packet_size(16#01B6) -> 114;
packet_size(16#01B7) -> 6;
packet_size(16#01B8) -> 3;
packet_size(16#01B9) -> 6;
packet_size(16#01BA) -> 26;
packet_size(16#01BB) -> 26;
packet_size(16#01BC) -> 26;
packet_size(16#01BD) -> 26;
packet_size(16#01BE) -> 2;
packet_size(16#01BF) -> 3;
packet_size(16#01C0) -> 2;
packet_size(16#01C1) -> 14;
packet_size(16#01C2) -> 10;
packet_size(16#01C3) -> 0;
packet_size(16#01C4) -> 22;
packet_size(16#01C5) -> 22;
packet_size(16#01C6) -> 4;
packet_size(16#01C7) -> 2;
packet_size(16#01C8) -> 13;
packet_size(16#01C9) -> 97;
packet_size(16#01CA) -> 3;
packet_size(16#01CB) -> 9;
packet_size(16#01CC) -> 9;
packet_size(16#01CD) -> 30;
packet_size(16#01CE) -> 6;
packet_size(16#01CF) -> 28;
packet_size(16#01D0) -> 8;
packet_size(16#01D1) -> 14;
packet_size(16#01D2) -> 10;
packet_size(16#01D3) -> 35;
packet_size(16#01D4) -> 6;
packet_size(16#01D5) -> 0;
packet_size(16#01D6) -> 4;
packet_size(16#01D7) -> 11;
packet_size(16#01D8) -> 54;
packet_size(16#01D9) -> 53;
packet_size(16#01DA) -> 60;
packet_size(16#01DB) -> 2;
packet_size(16#01DC) -> 0;
packet_size(16#01DD) -> 47;
packet_size(16#01DE) -> 33;
packet_size(16#01DF) -> 6;
packet_size(16#01E0) -> 30;
packet_size(16#01E1) -> 8;
packet_size(16#01E2) -> 34;
packet_size(16#01E3) -> 14;
packet_size(16#01E4) -> 2;
packet_size(16#01E5) -> 6;
packet_size(16#01E6) -> 26;
packet_size(16#01E7) -> 2;
packet_size(16#01E8) -> 28;
packet_size(16#01E9) -> 81;
packet_size(16#01EA) -> 6;
packet_size(16#01EB) -> 10;
packet_size(16#01EC) -> 26;
packet_size(16#01ED) -> 2;
packet_size(16#01EE) -> 0;
packet_size(16#01EF) -> 0;
packet_size(16#01F0) -> 0;
packet_size(16#01F1) -> 0;
packet_size(16#01F2) -> 20;
packet_size(16#01F3) -> 10;
packet_size(16#01F4) -> 32;
packet_size(16#01F5) -> 9;
packet_size(16#01F6) -> 34;
packet_size(16#01F7) -> 14;
packet_size(16#01F8) -> 2;
packet_size(16#01F9) -> 6;
packet_size(16#01FA) -> 48;
packet_size(16#01FB) -> 56;
packet_size(16#01FC) -> 0;
packet_size(16#01FD) -> 15;
packet_size(16#01FE) -> 5;
packet_size(16#01FF) -> 10;
packet_size(16#0200) -> 26;
packet_size(16#0201) -> 0;
packet_size(16#0202) -> 26;
packet_size(16#0203) -> 10;
packet_size(16#0204) -> 18;
packet_size(16#0205) -> 26;
packet_size(16#0206) -> 11;
packet_size(16#0207) -> 34;
packet_size(16#0208) -> 14;
packet_size(16#0209) -> 36;
packet_size(16#020A) -> 10;
packet_size(16#020D) -> 0;
packet_size(16#020E) -> 32;
packet_size(16#020F) -> 10;
packet_size(16#0210) -> 22;
packet_size(16#0212) -> 26;
packet_size(16#0213) -> 26;
packet_size(16#0214) -> 42;
packet_size(16#0215) -> 6;
packet_size(16#0216) -> 6;
packet_size(16#0217) -> 2;
packet_size(16#0218) -> 2;
packet_size(16#0219) -> 282;
packet_size(16#021A) -> 282;
packet_size(16#021B) -> 10;
packet_size(16#021C) -> 10;
packet_size(16#021D) -> 6;
packet_size(16#021E) -> 6;
packet_size(16#021F) -> 66;
packet_size(16#0220) -> 10;
packet_size(16#0221) -> 0;
packet_size(16#0222) -> 6;
packet_size(16#0223) -> 8;
packet_size(16#0224) -> 10;
packet_size(16#0225) -> 2;
packet_size(16#0226) -> 282;
packet_size(16#0227) -> 18;
packet_size(16#0228) -> 18;
packet_size(16#0229) -> 15;
packet_size(16#022A) -> 58;
packet_size(16#022B) -> 57;
packet_size(16#022C) -> 65;
packet_size(16#022D) -> 5;
packet_size(16#022E) -> 71;
packet_size(16#022F) -> 5;
packet_size(16#0230) -> 12;
packet_size(16#0231) -> 26;
packet_size(16#0232) -> 9;
packet_size(16#0233) -> 11;
packet_size(16#0234) -> 6;
packet_size(16#0235) -> 0;
packet_size(16#0236) -> 10;
packet_size(16#0237) -> 2;
packet_size(16#0238) -> 282;
packet_size(16#0239) -> 11;
packet_size(16#023A) -> 4;
packet_size(16#023B) -> 36;
packet_size(16#023C) -> 6;
packet_size(16#023D) -> 6;
packet_size(16#023E) -> 8;
packet_size(16#023F) -> 2;
packet_size(16#0240) -> 0;
packet_size(16#0241) -> 6;
packet_size(16#0242) -> 0;
packet_size(16#0243) -> 6;
packet_size(16#0244) -> 6;
packet_size(16#0245) -> 3;
packet_size(16#0246) -> 4;
packet_size(16#0247) -> 8;
packet_size(16#0248) -> 0;
packet_size(16#0249) -> 3;
packet_size(16#024A) -> 70;
packet_size(16#024B) -> 4;
packet_size(16#024C) -> 8;
packet_size(16#024D) -> 12;
packet_size(16#024E) -> 6;
packet_size(16#024F) -> 10;
packet_size(16#0250) -> 3;
packet_size(16#0251) -> 34;
packet_size(16#0252) -> 0;
packet_size(16#0253) -> 3;
packet_size(16#0254) -> 3;
packet_size(16#0255) -> 5;
packet_size(16#0256) -> 5;
packet_size(16#0257) -> 8;
packet_size(16#0258) -> 2;
packet_size(16#0259) -> 3;
packet_size(16#025A) -> 0;
packet_size(16#025B) -> 6;
packet_size(16#025C) -> 4;
packet_size(16#025D) -> 6;
packet_size(16#025E) -> 4;
packet_size(16#025F) -> 6;
packet_size(16#0260) -> 6;
packet_size(16#0261) -> 11;
packet_size(16#0262) -> 11;
packet_size(16#0263) -> 11;
packet_size(16#0264) -> 20;
packet_size(16#0265) -> 20;
packet_size(16#0266) -> 30;
packet_size(16#0267) -> 4;
packet_size(16#0268) -> 4;
packet_size(16#0269) -> 4;
packet_size(16#026A) -> 4;
packet_size(16#026B) -> 4;
packet_size(16#026C) -> 4;
packet_size(16#026D) -> 4;
packet_size(16#026F) -> 2;
packet_size(16#0270) -> 2;
packet_size(16#0271) -> 40;
packet_size(16#0272) -> 44;
packet_size(16#0273) -> 30;
packet_size(16#0274) -> 8;
packet_size(16#0275) -> 37;
packet_size(16#0276) -> 0;
packet_size(16#0277) -> 84;
packet_size(16#0278) -> 2;
packet_size(16#0279) -> 2;
packet_size(16#027A) -> 0;
packet_size(16#027B) -> 14;
packet_size(16#027C) -> 60;
packet_size(16#027D) -> 62;
packet_size(16#027E) -> 0;
packet_size(16#027F) -> 8;
packet_size(16#0280) -> 12;
packet_size(16#0281) -> 4;
packet_size(16#0282) -> 284;
packet_size(16#0283) -> 6;
packet_size(16#0284) -> 14;
packet_size(16#0285) -> 6;
packet_size(16#0286) -> 4;
packet_size(16#0287) -> 0;
packet_size(16#0288) -> 10;
packet_size(16#0289) -> 12;
packet_size(16#028A) -> 18;
packet_size(16#028B) -> 0;
packet_size(16#028C) -> 46;
packet_size(16#028D) -> 34;
packet_size(16#028E) -> 4;
packet_size(16#028F) -> 6;
packet_size(16#0290) -> 4;
packet_size(16#0291) -> 4;
packet_size(16#0292) -> 2;
packet_size(16#0293) -> 70;
packet_size(16#0294) -> 10;
packet_size(16#0295) -> 0;
packet_size(16#0296) -> 0;
packet_size(16#0297) -> 0;
packet_size(16#0298) -> 8;
packet_size(16#0299) -> 6;
packet_size(16#029A) -> 27;
packet_size(16#029B) -> 80;
packet_size(16#029C) -> 66;
packet_size(16#029D) -> 0;
packet_size(16#029E) -> 11;
packet_size(16#029F) -> 3;
packet_size(16#02A2) -> 8;
packet_size(16#02A5) -> 8;
packet_size(16#02AA) -> 4;
packet_size(16#02AB) -> 36;
packet_size(16#02AC) -> 6;
packet_size(16#02AD) -> 8;
packet_size(16#02B0) -> 85;
packet_size(16#02B1) -> 0;
packet_size(16#02B2) -> 0;
packet_size(16#02B3) -> 107;
packet_size(16#02B4) -> 6;
packet_size(16#02B5) -> 0;
packet_size(16#02B6) -> 7;
packet_size(16#02B7) -> 7;
packet_size(16#02B8) -> 22;
packet_size(16#02B9) -> 191;
packet_size(16#02BA) -> 11;
packet_size(16#02BB) -> 8;
packet_size(16#02BC) -> 6;
packet_size(16#02C1) -> 0;
packet_size(16#02C2) -> 0;
packet_size(16#02C4) -> 10;
packet_size(16#02C5) -> 30;
packet_size(16#02C6) -> 30;
packet_size(16#02C7) -> 7;
packet_size(16#02C8) -> 3;
packet_size(16#02C9) -> 3;
packet_size(16#02CA) -> 3;
packet_size(16#02CB) -> 65;
packet_size(16#02CC) -> 4;
packet_size(16#02CD) -> 71;
packet_size(16#02CE) -> 10;
packet_size(16#02CF) -> 6;
packet_size(16#02D0) -> 0;
packet_size(16#02D1) -> 0;
packet_size(16#02D2) -> 0;
packet_size(16#02D3) -> 4;
packet_size(16#02D4) -> 29;
packet_size(16#02D5) -> 2;
packet_size(16#02D6) -> 6;
packet_size(16#02D7) -> 0;
packet_size(16#02D8) -> 10;
packet_size(16#02D9) -> 10;
packet_size(16#02DA) -> 3;
packet_size(16#02DB) -> 0;
packet_size(16#02DC) -> 0;
packet_size(16#02DD) -> 32;
packet_size(16#02DE) -> 6;
packet_size(16#02DF) -> 36;
packet_size(16#02E0) -> 34;
packet_size(16#02E1) -> 33;
packet_size(16#02E2) -> 20;
packet_size(16#02E3) -> 22;
packet_size(16#02E4) -> 11;
packet_size(16#02E5) -> 9;
packet_size(16#02E6) -> 6;
packet_size(16#02E7) -> 0;
packet_size(16#02E8) -> 0;
packet_size(16#02E9) -> 0;
packet_size(16#02EA) -> 0;
packet_size(16#02EB) -> 13;
packet_size(16#02EC) -> 67;
packet_size(16#02ED) -> 59;
packet_size(16#02EE) -> 60;
packet_size(16#02EF) -> 8;
packet_size(16#02F0) -> 10;
packet_size(16#02F1) -> 2;
packet_size(16#02F2) -> 2;
packet_size(16#02F3) -> 0;
packet_size(16#02F4) -> 0;
packet_size(16#02F5) -> 0;
packet_size(16#02F6) -> 0;
packet_size(16#02F7) -> 0;
packet_size(16#02F8) -> 0;
packet_size(16#02F9) -> 0;
packet_size(16#02FA) -> 0;
packet_size(16#02FB) -> 0;
packet_size(16#02FC) -> 0;
packet_size(16#02FD) -> 0;
packet_size(16#02FE) -> 0;
packet_size(16#02FF) -> 0;
packet_size(16#0300) -> 0;
packet_size(16#0301) -> 0;
packet_size(16#0302) -> 0;
packet_size(16#0303) -> 0;
packet_size(16#0304) -> 0;
packet_size(16#0305) -> 0;
packet_size(16#0306) -> 0;
packet_size(16#0307) -> 0;
packet_size(16#0308) -> 0;
packet_size(16#0309) -> 0;
packet_size(16#030A) -> 0;
packet_size(16#030B) -> 0;
packet_size(16#030C) -> 0;
packet_size(16#030D) -> 0;
packet_size(16#030E) -> 0;
packet_size(16#030F) -> 0;
packet_size(16#0310) -> 0;
packet_size(16#0311) -> 0;
packet_size(16#0312) -> 0;
packet_size(16#0313) -> 0;
packet_size(16#0314) -> 0;
packet_size(16#0315) -> 0;
packet_size(16#0316) -> 0;
packet_size(16#0317) -> 0;
packet_size(16#0318) -> 0;
packet_size(16#0319) -> 0;
packet_size(16#031A) -> 0;
packet_size(16#031B) -> 0;
packet_size(16#031C) -> 0;
packet_size(16#031D) -> 0;
packet_size(16#031E) -> 0;
packet_size(16#031F) -> 0;
packet_size(16#0320) -> 0;
packet_size(16#0321) -> 0;
packet_size(16#0322) -> 0;
packet_size(16#0323) -> 0;
packet_size(16#0324) -> 0;
packet_size(16#0325) -> 0;
packet_size(16#0326) -> 0;
packet_size(16#0327) -> 0;
packet_size(16#0328) -> 0;
packet_size(16#0329) -> 0;
packet_size(16#032A) -> 0;
packet_size(16#032B) -> 0;
packet_size(16#032C) -> 0;
packet_size(16#032D) -> 0;
packet_size(16#032E) -> 0;
packet_size(16#032F) -> 0;
packet_size(16#0330) -> 0;
packet_size(16#0331) -> 0;
packet_size(16#0332) -> 0;
packet_size(16#0333) -> 0;
packet_size(16#0334) -> 0;
packet_size(16#0335) -> 0;
packet_size(16#0336) -> 0;
packet_size(16#0337) -> 0;
packet_size(16#0338) -> 0;
packet_size(16#0339) -> 0;
packet_size(16#033A) -> 0;
packet_size(16#033B) -> 0;
packet_size(16#033C) -> 0;
packet_size(16#033D) -> 0;
packet_size(16#033E) -> 0;
packet_size(16#033F) -> 0;
packet_size(16#0340) -> 0;
packet_size(16#0341) -> 0;
packet_size(16#0342) -> 0;
packet_size(16#0343) -> 0;
packet_size(16#0344) -> 0;
packet_size(16#0345) -> 0;
packet_size(16#0346) -> 0;
packet_size(16#0347) -> 0;
packet_size(16#0348) -> 0;
packet_size(16#0349) -> 0;
packet_size(16#034A) -> 0;
packet_size(16#034B) -> 0;
packet_size(16#034C) -> 0;
packet_size(16#034D) -> 0;
packet_size(16#034E) -> 0;
packet_size(16#034F) -> 0;
packet_size(16#0350) -> 0;
packet_size(16#0351) -> 0;
packet_size(16#0352) -> 0;
packet_size(16#0353) -> 0;
packet_size(16#0354) -> 0;
packet_size(16#0355) -> 0;
packet_size(16#0356) -> 0;
packet_size(16#0357) -> 0;
packet_size(16#0358) -> 0;
packet_size(16#0359) -> 0;
packet_size(16#035A) -> 0;
packet_size(16#035B) -> 0;
packet_size(16#035C) -> 2;
packet_size(16#035D) -> 0;
packet_size(16#035E) -> 2;
packet_size(16#035F) -> 0;
packet_size(16#0360) -> 0;
packet_size(16#0361) -> 0;
packet_size(16#0362) -> 0;
packet_size(16#0363) -> 0;
packet_size(16#0364) -> 5; %% packet(0x0364,5,clif->pWalkToXY,2);
packet_size(16#0365) -> 0;
packet_size(16#0366) -> 5; %% CZ_CHANGE_DIRECTION
packet_size(16#0367) -> 0;
packet_size(16#0368) -> 0;
packet_size(16#0369) -> 0;
packet_size(16#036A) -> 0;
packet_size(16#036B) -> 0;
packet_size(16#036C) -> 0;
packet_size(16#036D) -> 0;
packet_size(16#036E) -> 0;
packet_size(16#036F) -> 0;
packet_size(16#0370) -> 0;
packet_size(16#0371) -> 0;
packet_size(16#0372) -> 0;
packet_size(16#0373) -> 0;
packet_size(16#0374) -> 0;
packet_size(16#0375) -> 0;
packet_size(16#0376) -> 0;
packet_size(16#0377) -> 0;
packet_size(16#0378) -> 0;
packet_size(16#0379) -> 0;
packet_size(16#037A) -> 0;
packet_size(16#037B) -> 0;
packet_size(16#037C) -> 0;
packet_size(16#037D) -> 0;
packet_size(16#037E) -> 0;
packet_size(16#037F) -> 0;
packet_size(16#0380) -> 0;
packet_size(16#0381) -> 0;
packet_size(16#0382) -> 0;
packet_size(16#0383) -> 0;
packet_size(16#0384) -> 0;
packet_size(16#0385) -> 0;
packet_size(16#0386) -> 0;
packet_size(16#0387) -> 0;
packet_size(16#0388) -> 0;
packet_size(16#0389) -> 0;
packet_size(16#038A) -> 0;
packet_size(16#038B) -> 0;
packet_size(16#038C) -> 0;
packet_size(16#038D) -> 0;
packet_size(16#038E) -> 0;
packet_size(16#038F) -> 0;
packet_size(16#0390) -> 0;
packet_size(16#0391) -> 0;
packet_size(16#0392) -> 0;
packet_size(16#0393) -> 0;
packet_size(16#0394) -> 0;
packet_size(16#0395) -> 0;
packet_size(16#0396) -> 0;
packet_size(16#0397) -> 0;
packet_size(16#0398) -> 0;
packet_size(16#0399) -> 0;
packet_size(16#039A) -> 0;
packet_size(16#039B) -> 0;
packet_size(16#039C) -> 0;
packet_size(16#039D) -> 0;
packet_size(16#039E) -> 0;
packet_size(16#039F) -> 0;
packet_size(16#03A0) -> 0;
packet_size(16#03A1) -> 0;
packet_size(16#03A2) -> 0;
packet_size(16#03A3) -> 0;
packet_size(16#03A4) -> 0;
packet_size(16#03A5) -> 0;
packet_size(16#03A6) -> 0;
packet_size(16#03A7) -> 0;
packet_size(16#03A8) -> 0;
packet_size(16#03A9) -> 0;
packet_size(16#03AA) -> 0;
packet_size(16#03AB) -> 0;
packet_size(16#03AC) -> 0;
packet_size(16#03AD) -> 0;
packet_size(16#03AE) -> 0;
packet_size(16#03AF) -> 0;
packet_size(16#03B0) -> 0;
packet_size(16#03B1) -> 0;
packet_size(16#03B2) -> 0;
packet_size(16#03B3) -> 0;
packet_size(16#03B4) -> 0;
packet_size(16#03B5) -> 0;
packet_size(16#03B6) -> 0;
packet_size(16#03B7) -> 0;
packet_size(16#03B8) -> 0;
packet_size(16#03B9) -> 0;
packet_size(16#03BA) -> 0;
packet_size(16#03BB) -> 0;
packet_size(16#03BC) -> 0;
packet_size(16#03BD) -> 0;
packet_size(16#03BE) -> 0;
packet_size(16#03BF) -> 0;
packet_size(16#03C0) -> 0;
packet_size(16#03C1) -> 0;
packet_size(16#03C2) -> 0;
packet_size(16#03C3) -> 0;
packet_size(16#03C4) -> 0;
packet_size(16#03C5) -> 0;
packet_size(16#03C6) -> 0;
packet_size(16#03C7) -> 0;
packet_size(16#03C8) -> 0;
packet_size(16#03C9) -> 0;
packet_size(16#03CA) -> 0;
packet_size(16#03CB) -> 0;
packet_size(16#03CC) -> 0;
packet_size(16#03CD) -> 0;
packet_size(16#03CE) -> 0;
packet_size(16#03CF) -> 0;
packet_size(16#03D0) -> 0;
packet_size(16#03D1) -> 0;
packet_size(16#03D2) -> 0;
packet_size(16#03D3) -> 0;
packet_size(16#03D4) -> 0;
packet_size(16#03D5) -> 0;
packet_size(16#03D6) -> 0;
packet_size(16#03D7) -> 0;
packet_size(16#03D8) -> 0;
packet_size(16#03D9) -> 0;
packet_size(16#03DA) -> 0;
packet_size(16#03DB) -> 0;
packet_size(16#03DC) -> 0;
packet_size(16#03DD) -> 18;
packet_size(16#03DE) -> 18;
packet_size(16#03E2) -> 0;
packet_size(16#03E3) -> 0;
packet_size(16#03E4) -> 0;
packet_size(16#03E5) -> 0;
packet_size(16#03E6) -> 0;
packet_size(16#03E7) -> 0;
packet_size(16#03E8) -> 0;
packet_size(16#03E9) -> 0;
packet_size(16#03EA) -> 0;
packet_size(16#03EB) -> 0;
packet_size(16#03EC) -> 0;
packet_size(16#03ED) -> 0;
packet_size(16#03EE) -> 0;
packet_size(16#03EF) -> 0;
packet_size(16#03F0) -> 0;
packet_size(16#03F1) -> 0;
packet_size(16#03F2) -> 0;
packet_size(16#03F3) -> 0;
packet_size(16#03F4) -> 0;
packet_size(16#03F5) -> 0;
packet_size(16#03F6) -> 0;
packet_size(16#03F7) -> 0;
packet_size(16#03F8) -> 0;
packet_size(16#03F9) -> 0;
packet_size(16#03FA) -> 0;
packet_size(16#03FB) -> 0;
packet_size(16#03FC) -> 0;
packet_size(16#03FD) -> 0;
packet_size(16#03FE) -> 0;
packet_size(16#03FF) -> 0;
packet_size(16#0400) -> 0;
packet_size(16#0401) -> 0;
packet_size(16#0402) -> 0;
packet_size(16#0403) -> 0;
packet_size(16#0404) -> 0;
packet_size(16#0405) -> 0;
packet_size(16#0406) -> 0;
packet_size(16#0407) -> 0;
packet_size(16#0408) -> 0;
packet_size(16#0409) -> 0;
packet_size(16#040A) -> 0;
packet_size(16#040B) -> 0;
packet_size(16#040C) -> 0;
packet_size(16#040D) -> 0;
packet_size(16#040E) -> 0;
packet_size(16#040F) -> 0;
packet_size(16#0410) -> 0;
packet_size(16#0411) -> 0;
packet_size(16#0412) -> 0;
packet_size(16#0413) -> 0;
packet_size(16#0414) -> 0;
packet_size(16#0415) -> 0;
packet_size(16#0416) -> 0;
packet_size(16#0417) -> 0;
packet_size(16#0418) -> 0;
packet_size(16#0419) -> 0;
packet_size(16#041A) -> 0;
packet_size(16#041B) -> 0;
packet_size(16#041C) -> 0;
packet_size(16#041D) -> 0;
packet_size(16#041E) -> 0;
packet_size(16#041F) -> 0;
packet_size(16#0420) -> 0;
packet_size(16#0421) -> 0;
packet_size(16#0422) -> 0;
packet_size(16#0423) -> 0;
packet_size(16#0424) -> 0;
packet_size(16#0425) -> 0;
packet_size(16#0426) -> 0;
packet_size(16#0427) -> 0;
packet_size(16#0428) -> 0;
packet_size(16#0429) -> 0;
packet_size(16#042A) -> 0;
packet_size(16#042B) -> 0;
packet_size(16#042C) -> 0;
packet_size(16#042D) -> 0;
packet_size(16#042E) -> 0;
packet_size(16#042F) -> 0;
packet_size(16#0430) -> 0;
packet_size(16#0431) -> 0;
packet_size(16#0432) -> 0;
packet_size(16#0433) -> 0;
packet_size(16#0434) -> 0;
packet_size(16#0435) -> 0;
packet_size(16#0436) -> 19;
packet_size(16#0437) -> 7;
packet_size(16#0438) -> 10;
packet_size(16#0439) -> 8;
packet_size(16#043D) -> 8;
packet_size(16#043E) -> 0;
packet_size(16#043F) -> 25;
packet_size(16#0440) -> 10;
packet_size(16#0441) -> 4;
packet_size(16#0442) -> 0;
packet_size(16#0443) -> 8;
packet_size(16#0444) -> 0;
packet_size(16#0445) -> 10;
packet_size(16#0446) -> 14;
packet_size(16#0447) -> 2;
packet_size(16#0448) -> 0;
packet_size(16#044A) -> 6;
packet_size(16#044B) -> 2;
packet_size(16#07D0) -> 6;
packet_size(16#07D1) -> 2;
packet_size(16#07D2) -> 0;
packet_size(16#07D3) -> 4;
packet_size(16#07D4) -> 4;
packet_size(16#07D5) -> 4;
packet_size(16#07D6) -> 4;
packet_size(16#07D7) -> 8;
packet_size(16#07D8) -> 8;
packet_size(16#07D9) -> 268;
packet_size(16#07DA) -> 6;
packet_size(16#07DB) -> 8;
packet_size(16#07DC) -> 6;
packet_size(16#07DD) -> 54;
packet_size(16#07DE) -> 30;
packet_size(16#07DF) -> 54;
packet_size(16#07E0) -> 58;
packet_size(16#07E1) -> 15;
packet_size(16#07E2) -> 8;
packet_size(16#07E3) -> 6;
packet_size(16#07E4) -> 0;
packet_size(16#07E6) -> 28;
packet_size(16#07E7) -> 5;
packet_size(16#0815) -> 6;
packet_size(16#0817) -> 6;
packet_size(16#082d) -> 29;
packet_size(16#0885) -> 6;
packet_size(16#088a) -> 6;
packet_size(16#08aa) -> 7;
packet_size(16#08B9) -> 12;
packet_size(16#08c8) -> 34;
packet_size(16#099d) -> 0;
packet_size(16#0A39) -> 35;
packet_size(16#0AC4) -> 0; %% HEADER_AC_ACCEPT_LOGIN2, variable length
packet_size(_Other)  -> undefined.
