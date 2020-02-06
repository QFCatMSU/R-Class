{
  rm(list=ls());                        # clean out the environment
  options(show.error.locations = TRUE); # give line numbers on errors
  
  fish1Weight = 45;
  fish2Weight = 64;
  fish3Weight = 50;
  fish4Weight = 58;
  fish5Weight = 49;
  
  # solve for the mean fish weight
  meanWeight = (fish1Weight + fish2Weight + fish3Weight + fish4Weight + fish5Weight) /5;
  
  # solve the variance for each fish
  fish1Var = (fish1Weight - meanWeight)^2;
  fish2Var = (fish2Weight - meanWeight)^2;
  fish3Var = (fish3Weight - meanWeight)^2;
  fish4Var = (fish4Weight - meanWeight)^2;
  fish5Var = (fish5Weight - meanWeight)^2;
  
  # find the variance of the sample
  weightVariance = (fish1Var + fish2Var + fish3Var + fish4Var + fish5Var) / 5;
  
  # solve for the standard deviation in the fish weight
  weightStandardDev = (weightVariance) ^ (1/2);
  
  # solve for the 95% confidence interval 
  Z = 1.960;
  lowEnd = meanWeight - Z*(weightStandardDev / 5^2);
  highEnd = meanWeight + Z*(weightStandardDev / 5^2);
}

#### Issues
# GENERAL:
# 1) No semicolons at the end of lines
#
# SYNTAX:
# 2) line 12: Fish3Weight should be fish3Weight (lower f)
# 3) line 18: meanWeihgt should be meanWeight
# 4) line 22: fishvar5 should be fish5Var
# 5) line 25: == should be = (this is a very common error as both are valid operators)
# 6) line 28, 29: z should be Z (or line 28: Z should be z)
# 7) line 28, 29: need multiplication symbol -- 1.960*(weight...)
#
# LOGIC:
# 8) line 12: parentheses need around fish1Weight +...+fish4Weight
# 9) lines 16-19: all use fish1Weight, should use fish2Weight ... fish5Weight
# 10) line 22: closing parenthesis in wrong place, should be after fish5Var
# 11) line 25: parenthesis needed around 1/2 
####
  
  
  
  
  
  
  
  