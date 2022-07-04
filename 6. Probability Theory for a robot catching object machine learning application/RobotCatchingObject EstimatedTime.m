classdef RobotCatchingObject
    methods(Static)
        function [ExpectedTime_CatchingObj_S1] = vpara_P2_Strategy1(q)        
        % Strategy1- the robot moves 1 unit towards the object irrespective of the object movement
        % PoM(as an function argument q):0.1,0.2,0.3
        % the extected time is calculated using the total expectation theorm
            E1 = [];                                                       % Initiate the expected value list 
            ExpectedTime_CatchingObj_S1 = 0;                               % Initiate the expected time to zero
            pmf_d = 0.1;

            E1(1) = (1 /(1-q)) ;                                           % the expected value given one unit time movement of robot
            E1(2) = (2-(3 * q)) / (power((1-q),2));                        % the expected value given two unit time movement of robot
            E1(3) = ( 3 - (9 * q) + (6 * q * q) ) / (power((1-q),3));      % the expected value given three unit time movement of robot
            for d = 4:19                                                   % the expected value given n unit time movement of robot, 
                                                                           % where n is an integer greater than 3                                                      
                E1(d) = (q + q*(1+E1(d-2)) + (1-(2*q)) * E1(d-1)) / (1-q);       
            end                                                                
            for d = 10:19                                                  % calculating the expected time to catch the object 
                                                                           % for all the possible distances apart as per the project description
                ExpectedTime_CatchingObj_S1 = ExpectedTime_CatchingObj_S1 + E1(d) * pmf_d;        
            end                                                                

        end                                                                 

        function [ExpectedTime_CatchingObj_S3] = vpara_P2_Strategy3(q)     
        % Strategy3- the robot moves 1 unit towards the object when the object moves either left or right 
        % PoM(as an function argument q):0.1,0.2,0.3
        % the extected time is calculated using the total expectation theorm
            E3 = [];                                                       % Initiate the expected value list 
            ExpectedTime_CatchingObj_S3 = 0;                               % Initiate the expected time to zero
            pmf_d = 0.1;

            for d = 10:19                                                   % the expected value given n unit time movement of robot, 
                                                                           % where n is an integer greater than 3 
                if rem(d,2) == 0                                                            
                    E3(d) = d/(2*q);
                else
                    E3(d) = (d+1)/(2*q); 
                end

            end                                                                
            for d = 10:19                                                  % calculating the expected time to catch the object 
                                                                           % for all the possible distances apart as per the project description
                ExpectedTime_CatchingObj_S3 = ExpectedTime_CatchingObj_S3 + E3(d) * pmf_d;        
            end                                                                

        end    
        function [ExpectedTime_CatchingObj_S2] = vpara_P2_Strategy2(q)         
        % Strategy2- the robot moves 1 unit towards the object or stops regardless of the object movement
        % PoM(as an function argument q):0.1,0.2,0.3
        % the extected time is calculated using the total expectation theorm


            E2 = [];                                                       % Initiate the expected value list 
            ExpectedTime_CatchingObj_S2 = 0;                               % Initiate the expected time to zero
            pmf_d = 0.1;

            %eqn1 = ((4.5*q*q)-(3.5*q))*E1 - ((1.5*q*q)-q)*E2 == -1;
            %eqn2 = ((2.5*q)-(4.5*q*q))*E1 - ((3.5*q)-(4.5*q*q))*E2 + (q-(1.5*q*q))*E3 == -1;
            %eqn3 = E2 - E3 == 0;
            % the system of equations are constructed using the matrix A and B
            A=[((4.5*q*q)-(3.5*q)) -((1.5*q*q)-q) 0; ((2.5*q)-(4.5*q*q)) -((3.5*q)-(4.5*q*q)) (q-(1.5*q*q)); 0 1 -1];
            B = [-1; -1; 0];                                            
            X = A\B;                                                       % A inverse B   
            E2(1) = X(1,1);
            E2(2) = X(2,1);
            E2(3) = X(3,1);

            for d = 3:19                                                   % the expected value given n unit time movement of robot, 
                                                                           % where n is an integer greater than 3  
                                                             
                E2(d) = ( (((2.5*q)-(4.5*q*q)) * E2(d-1)) + 1) / ((2.5*q)-(3.0*q*q));  % it is very complex to find out the generalized equation hence approximated   
            end                                                                
            for d = 10:19                                                  % calculating the expected time to catch the object 
                                                                           % for all the possible distances apart as per the project description
                ExpectedTime_CatchingObj_S2 = ExpectedTime_CatchingObj_S2 + E2(d) * pmf_d;        
            end                                                                


        end             

   end
end
