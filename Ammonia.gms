Sets
    i               'Set of all units in the model'     /SMR,GAS,ELE,HABER,ELE_CAT,BAZ,SNK_CO2, SNK_BB,SNK_UREA,SNK_W,SNK_AMM,SNK_O2,METs,WATs,BIOs,N2s,CO2s/
        IREA(i)     'Set of reactors'                   /SMR,GAS,ELE,HABER,ELE_CAT,BAZ/
        ISNK(i)     'Set of sinks'                      /SNK_CO2, SNK_BB,SNK_UREA,SNK_W,SNK_AMM,SNK_O2/
        ISRC(i)     'Set of Sources'                    /METs,WATs,BIOs,N2s,CO2s/
    j               'Set of Streams'                    /1,2,3,4,5,6,8,9,11,12,13,14,15,16,17,18,19,20,21, 22, 23, 24, 25, 26,27/
        jIn(i,j)    'Set of inlet streams'              /SMR.1
                                                         SMR.2
                                                         GAS.3
                                                         GAS.27
                                                         ELE.4
                                                         HABER.9
                                                         HABER.12
                                                         HABER.13
                                                         HABER.15
                                                         ELE_CAT.5
                                                         ELE_CAT.6
                                                         BAZ.19
                                                         BAZ.16
                                                         BAZ.17
*                                                         BAZ.7
                                                         SNK_CO2.8
                                                         SNK_CO2.20
                                                         SNK_BB.11
                                                         SNK_UREA.24
                                                         SNK_W.23
                                                         SNK_W.25
                                                         SNK_W.26
                                                         SNK_AMM.21
                                                         SNK_AMM.22
                                                         SNK_O2.14
                                                         SNK_O2.18/
        jOut(i,j)   'Set of outlet Streams'             /METs.1
                                                         WATs.2
                                                         WATs.4
                                                         WATs.6
                                                         WATs.27
                                                         BIOs.3
                                                         N2s.5
                                                         N2s.15
                                                         CO2s.7
                                                         SMR.8
                                                         SMR.9
                                                         SMR.19
                                                         SMR.26
                                                         GAS.11
                                                         GAS.12
                                                         GAS.20
                                                         GAS.23
                                                         ELE.13
                                                         ELE.14
                                                         HABER.16
                                                         HABER.21
                                                         ELE_CAT.17
                                                         ELE_CAT.18
                                                         ELE_CAT.22
                                                         BAZ.24
                                                         BAZ.25/

   
    k               'Set of components'                 /MET,WAT,BIO,N2,H2,AMM,CO2, ASH, C, S, O2, CL2, UREA, BB/
        kr(k,i)     'Set of limiting reactants'         /MET.SMR
                                                         BIO.GAS
                                                         WAT.ELE
                                                         N2.HABER
                                                         WAT.ELE_CAT
                                                         AMM.BAZ/

    utilitydata     'Auxilary set for utility def'      /flow_ref,e_ref,qh_ref,qc_ref,cc_ref/
;
Parameters
    inlet(i,j)      'Auxiliar set for inlet streams'
    outlet(i,j)     'Auxiliar set for outlet streams'
    limiting(i,k)   'Auxiliar set for limiting ractants'
    e_consump(i)    'Electricity consumption in of stream i'
    h_consump(i)    'Hot utility consumption of i'
    c_consump(i)    'Cold utility consumption of i'
    epsilon(j,k)  'Seperation factor to exit stream j'  /1.MET = 1
                                                         2.WAT = 0.333
                                                         4.WAT = 0.333
                                                         6.WAT = 0.334
                                                         3.BIO = 1
                                                         15.N2 = 0.5
                                                         5.N2 = 0.5
                                                         7.CO2 = 1/
    af              'Annualization factor'             /0.1061/
    yi(i,k)       'Reaction Yield for component k'   /  SMR.H2 = 0.9063392857
                                                        SMR.CO2 = 2.088660714
                                                        SMR.MET = -1
                                                        SMR.WAT = -1.995
                                                        
                                                        GAS.H2 = 0.190192
                                                        GAS.CO2 = 1.03
                                                        GAS.BB = 0.226346154
                                                        GAS.WAT = -0.44654
                                                        GAS.BIO = -1
        
                                                        ELE.H2 = 0.018888888889
                                                        ELE.O2 = 0.14
                                                        ELE.WAT = -0.99778
                                                        
                                                        HABER.AMM = 1.125
                                                        HABER.H2 = -.125
                                                        HABER.N2 = -1
                                                        

                                                        ELE_CAT.H2 = 0.3
                                                        ELE_CAT.O2 = 0.2
*                                                       *arbitrary number, replace

                                                        BAZ.UREA = 1.435441
                                                        BAZ.WAT = 1.017967
                                                        BAZ.CO2 = -1.453467207
                                                        BAZ.AMM = -1
                                                        /
                                                        
    mw(k)           'Molecular Weight'                  /MET = 16
                                                        WAT = 18
                                                        Bio = 25
                                                        N2 = 28
                                                        H2 = 2
                                                        AMM = 17/
    e_price         'Electricity price [$/kJ]'          //
    c_price         'Electricity price [$/kJ]'          //
    h_price         'Electricity price [$/kJ]'          //
    price_prod      'Product price     [$/kg]'          //
    feed_price      'Feedstock price   [$/kg]'          //
    tf              'Time factor       [s/yr]'          /31563000/                    
    M               'Big M constraint'                  /50000/
;

      

Variables
    Z;
Binary Variables
    Y(i);
Positive Variables
    FM(j)               'Mass flow on stream j'
    F(j,k)              'Mass flow rate of component k on stream j'
    FR(i)             'Mass flow rate of limiting reactant in reaction r on catalyst i'

    Electricity(i)      'Electricity consumed in superstructure element i'
    QH(i)               'Hot utility consumption in equipment i'
    QC(i)               'Cold utility consumption in equipment i'
    CC(i)               'Capital cost of equipment i'
;

Equations
Objective                                           'NPV'
mass_flow_rate(j)                                   'Mol flow rate on stream j'
*Mol_balance_SNK(i,k)                                'Mol balance at splitters'
Big_m_constraint(i,k)                               'Big M constraint'
selection_constraint                                'Select only one system' 
Mol_Balance_Reactors(i,k)                           'Mol balance at reactors'
*Mol_Balance_SRC(i,k,j)                              'Mol balance at Source'
Limiting_reactant(i)
*Other_reactant(i,r)
*Electricity_consumption(i)
*Hot_consumption(i)
*Cold_consumption(i)

*capital_cost_equipment(i)

capacity

composition_MET_feed1
composition_MET_feed2
composition_MET_feed3
composition_WAT_feed
composition_WAT_feed2
composition_WAT_feed3
composition_BIO_feed1

*composition_CO2_feed
composition_N2_feed
composition_N2_feed2
;

Objective..                                                   Z=e=1;
mass_flow_rate(j)..                                           FM(j)=e=sum[k,F(j,k)];
Limiting_reactant(i)$[IREA(i)]..                             FR(i)=e=sum[k$kr(k,i),sum(j$jIn(i,j),F(j,k))];
Mol_Balance_Reactors(i,k)$[IREA(i)]..                     sum[j$jOut(i,j),F(j,k)]=e=sum[j$jIn(i,j),F(j,k)]+yi(i,k)*FR(i);
selection_constraint..                                        sum[i$IREA(i),Y(i)]=g=1;
Big_m_constraint(i,k)$[IREA(i)]..                             sum[j$jIN(i,j),F(j,k)] =l= M*Y(i);

capacity..                                                    FM('24') =e= 1000;

composition_MET_feed1..                                       F('1','MET')=e=0.94*FM('1');
composition_MET_feed2..                                       F('1','CO2')=e=0.01*FM('1');
composition_MET_feed3..                                       F('1','N2')=e=0.05*FM('1');

composition_WAT_feed..                                        F('2','WAT')=e=1*FM('2');
composition_WAT_feed2..                                       F('4','WAT')=e=1*FM('4');
composition_WAT_feed3..                                       F('6','WAT')=e=1*FM('6');



composition_BIO_feed1..                                       F('3','BIO')=e=FM('3');


composition_CO2_feed..                                        F('7','CO2')=e=1*FM('7');

composition_N2_feed..                                         F('15','N2')=e=1*FM('15');
composition_N2_feed2..                                        F('5','N2')=e=1*FM('5');




MODEL Superstructure /all/;
Superstructure.optfile = 1;
option optcr  = 0.001;
option limrow= 4000;
option MINLP=baron;
SOLVE Superstructure using minlp minimizing Z;



