function k9s --description 'alias k9s=k9s --context (kubie info ctx) --namespace (kubie info ns)'
 command k9s --context (kubie info ctx) --namespace (kubie info ns) $argv; 
end
