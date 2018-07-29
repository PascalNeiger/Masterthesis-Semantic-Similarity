function [ workingMemoryWord1,workingMemoryWord2,lcs ] = JustMarriedGermaNetWord2VecForR(~)

%Welcome to the function JustMarriedGermaNetWord2Vec
%First, this function is able to compare to words and calculate the similarity
%between them. Second, the function is able to generate respectively
%predict multiple words based on a given word and a given similarity.
%To use the function, you have four variables to use.
%If you only want to make a comparison, you have to enter the two words on the
%first and second position in the brackets and define the thrid and fourth
%variable as 'none'. If you only want to make a prediction, you need to set
%the first and second variable to 'none' and the third variable is the word
%on which you want to generate other words and the fourth input is the
%desired similarity that the generated words should have to the input word.
%To do a comparison and a prediciton, you just enter all the variables with
%your desired values.
%To use the function, you need the files GermaNetMatlab.mat,
%RelationsMatlab.mat, frequencies.mat and VektorenUndZuordnung.mat to be in the same folder
%as this function and that folder is your current directory.

load('GermaNetMatlab.mat') %Here the database of GermaNet is loaded
load('RelationsMatlab.mat') %Here we load the relationsfile. It is the backbone of GermaNet, since it is the databse for the whole hierarchy.
load('VektorenUndZuordnung.mat') %Here we load the vectors of the Word2Vec algorithm and a list, which contains the assignements of the words to their synset number.
load('frequencies.mat') %At last we load the frequencies for each synset

filename='WortlisteOnlineStudie.xlsx';

%Now we read in the wordlist.
[~,wordList] = xlsread(filename);

%And we creat fieldnames for the struct which will contain the endresults
%of the function.
for aMain=1:length(wordList(:,1))
    lineNumber=num2str(aMain);
    fieldName=strcat('line',lineNumber);
    fieldnamesOutputStruct{aMain}=fieldName;
end

%Here we have the main loop of this function, where all the subfunctions
%come together. The loop will go from line to line and the words are first
%transferred to a temporal variable.
for aMain=1:length(wordList(:,1))
    wordsToCalculate=wordList(aMain,:); %
    %wordsToCalculate=replace(wordsToCalculate,'ae','ä');%Now we replace the special letters.
    %wordsToCalculate=replace(wordsToCalculate,'ue','ü');
    %wordsToCalculate=replace(wordsToCalculate,'oe','ö');
    %wordsToCalculate=replace(wordsToCalculate,'ß','ss');
    %In the next step, we check if there are missing values.
    if isempty(wordsToCalculate{1,1})==1
        finalOutput{1,3}='No calculation possible because of missing value(s)';
        finalOutput(1,1:2)=wordsToCalculate;
        for zMain=1:17
            finalOutput{1,(zMain+3)}='NaN';
        end
        finalOutputStruct.(fieldnamesOutputStruct{aMain}){1,:}=finalOutput;
        continue
    elseif isempty(wordsToCalculate{1,2})==1
        finalOutput{1,3}='No calculation possible because of missing value(s)';
        finalOutput(1,1:2)=wordsToCalculate;
        for zMain=1:17
            finalOutput{1,(zMain+3)}='NaN';
        end
        finalOutputStruct.(fieldnamesOutputStruct{aMain}){1,:}=finalOutput;
        continue
    else
        finalOutput(1,1:2)=wordsToCalculate;
    end
    %Now we check if the word is contained in GermaNet. First we look if we
    %find it the way the user wrote it. Then we transform the first letter
    %in lower case and check again. And if this doesn´t work, we check if
    %the word is found if the first letter is in upper case. Why do both?
    %We can not foresee if the input is written in upper or lower case.
    if isempty(find(strcmp([zuordnung(:,1)],wordsToCalculate(1,1))))==1
        wordsToCalculate{1,1}=regexprep(wordsToCalculate{1,1},'(\<\w)','${lower($1)}');
        if isempty(find(strcmp([zuordnung(:,1)],wordsToCalculate(1,1))))==1
            wordsToCalculate{1,1}=regexprep(wordsToCalculate{1,1},'(\<\w)','${upper($1)}');
            if isempty(find(strcmp([zuordnung(:,1)],wordsToCalculate(1,1))))==1
                finalOutput{1,3}='At least one word is not included in GermaNet';
                for zMain=1:17
                    finalOutput{1,(zMain+3)}='NaN';
                end
                finalOutputStruct.(fieldnamesOutputStruct{aMain})(1,:)=finalOutput;
                continue
            end
        end
        finalOutput{1,1}=wordsToCalculate{1,1};
    elseif isempty(find(strcmp([zuordnung(:,1)],wordsToCalculate(1,2))))==1
        wordsToCalculate{1,2}=regexprep(wordsToCalculate{1,2},'(\<\w)','${lower($1)}');
        if isempty(find(strcmp([zuordnung(:,1)],wordsToCalculate(1,2))))==1
            wordsToCalculate{1,1}=regexprep(wordsToCalculate{1,1},'(\<\w)','${upper($1)}');
            if isempty(find(strcmp([zuordnung(:,1)],wordsToCalculate(1,2))))==1
                finalOutput{1,3}='At least one word is not included in the germaNet';
                for zMain=1:17
                    finalOutput{1,(zMain+3)}='NaN';
                end
                finalOutputStruct.(fieldnamesOutputStruct{aMain})(1,:)=finalOutput;
                continue
            end
        end
        finalOutput{1,2}=wordsToCalculate{1,2};
    end
    disp(aMain)
    %Now the first subfunction is called. As you will see, this subfunction
    %extracts all the variables that we need for the calculations.
    [workingMemoryWord1,fieldnamesWorkingMemoryWord1,workingMemoryWord2,fieldnamesWorkingMemoryWord2,lcs]=...
        getVariables(finalOutput{1,1},finalOutput{1,2},frequencies,germaNet,hierarchy,vectors,zuordnung);
    %The next lines are only for the export of the variables to do the
    %crosvalidation.
    fieldnameExport=strcat(wordsToCalculate{1},'_',wordsToCalculate{2});
    fieldnameExport=replace(fieldnameExport,'ä','ae');
    fieldnameExport=replace(fieldnameExport,'ö','oe');
    fieldnameExport=replace(fieldnameExport,'ü','ue');
    exportVariables(1).(fieldnameExport)=workingMemoryWord1;
    exportVariables(2).(fieldnameExport)=fieldnamesWorkingMemoryWord1;
    exportVariables(3).(fieldnameExport)=workingMemoryWord2;
    exportVariables(4).(fieldnameExport)=fieldnamesWorkingMemoryWord2;
    exportVariables(5).(fieldnameExport)=lcs;
    %In the next step we prepare the information gained of the subfunction
    %getVariables for the calculations.
    countLCS=1;
    countTempOutput=3;
    %We loop over every lexUnit of word 1 and compare it to every lexUnit of Word 2 with the second loop. 
    for bMain=1:length(fieldnamesWorkingMemoryWord1(1,:))
        for cMain=1:length(fieldnamesWorkingMemoryWord2(1,:))
            %In this if condition we check if both lexUnits have a vector,
            %else they go into the calculations without vectors.
            if isnumeric(workingMemoryWord1.(fieldnamesWorkingMemoryWord1{bMain}){1,2})==1&&...
                    isnumeric(workingMemoryWord2.(fieldnamesWorkingMemoryWord2{cMain}){1,2})==1
                compareWord1{1}=wordsToCalculate{1};
                compareWord1{2}=fieldnamesWorkingMemoryWord1{bMain};
                compareWord1{3}=workingMemoryWord1.frequencies.(fieldnamesWorkingMemoryWord1{bMain});
                compareWord1{4}=workingMemoryWord1.(fieldnamesWorkingMemoryWord1{bMain}){1,2};
                compareWord2{1}=wordsToCalculate{2};
                compareWord2{2}=fieldnamesWorkingMemoryWord2{cMain};
                compareWord2{3}=workingMemoryWord2.frequencies.(fieldnamesWorkingMemoryWord2{cMain});
                compareWord2{4}=workingMemoryWord2.(fieldnamesWorkingMemoryWord2{cMain}){1,2};
                compareLCS(1,:)=lcs{countLCS}(1,:);
                [ tempFinalOutput ]=comparsionGermaNetW2V(compareWord1,compareWord2,compareLCS);
            else
                compareWord1{1}=wordsToCalculate{1};
                compareWord1{2}=fieldnamesWorkingMemoryWord1{bMain};
                compareWord1{3}=workingMemoryWord1.frequencies.(fieldnamesWorkingMemoryWord1{bMain});
                compareWord2{1}=wordsToCalculate{2};
                compareWord2{2}=fieldnamesWorkingMemoryWord2{cMain};
                compareWord2{3}=workingMemoryWord2.frequencies.(fieldnamesWorkingMemoryWord2{cMain});
                compareLCS{1}=lcs{countLCS};
                [ tempFinalOutput ]=comparsionGermaNet(compareWord1,compareWord2,compareLCS);
            end
            %In the next loop we begin to prepare the output in form of a
            %cell.
            for dMain=1:length(tempFinalOutput)                
                if countTempOutput==3
                    finalOutput{(countTempOutput-1),dMain}=tempFinalOutput{1,dMain};
                    finalOutput{countTempOutput,dMain}=tempFinalOutput{2,dMain};                    
                else
                    finalOutput{countTempOutput,dMain}=tempFinalOutput{2,dMain};
                end               
            end           
            countTempOutput=countTempOutput+1;
            countLCS=countLCS+1;
        end
    end
    %This is the addition to choose only the best comparsion.
    maxValues=cellfun(@max,finalOutput(3:end,11));
    [rowMaxValue,~]=find(maxValues==max(maxValues));
    if length(rowMaxValue)>1
        rowMaxValue=rowMaxValue(1);
    end
    if aMain==1
        maximumValue(1,:)=finalOutput(2,:);
        maximumValue(2,:)=finalOutput((rowMaxValue+2),:);
    else
        maximumValue(1,:)=finalOutput((rowMaxValue+2),:);
    end
    %The whole cell we generated above now is stored to a struct we then
    %will transform in a cell where all comparsions of all lexUnits of all
    %the words of the input list are printed.
    lineNRFinalStruct=num2str(aMain);
    fieldnameFinalStruct=strcat('line',lineNRFinalStruct);
    finalOutputStruct.(fieldnameFinalStruct)=maximumValue;
    finalOutput={};
    maximumValue={};
end

fieldnamesOutputStruct=fieldnames(finalOutputStruct);

%Here we create two files containing the words with their corresponding
%vectors.
for fMain=1:length(wordList(:,1))
    tempWord1=wordList(fMain,1);
    tempWord2=wordList(fMain,2);
    vectorsForExcelWord1{fMain,1}=tempWord1;
    vectorsForExcelWord2{fMain,1}=tempWord2;
    rowVectorsWord1=find(strcmp([vectors(:,1)],tempWord1));
    rowVectorsWord2=find(strcmp([vectors(:,1)],tempWord2));
    if isempty(rowVectorsWord1)==1
        vectorsForExcelWord1{fMain,2}='There is no vector for this word';
    else
        tempVec1=vectors{rowVectorsWord1(1),2};
        for gMain=1:length(tempVec1)
            tempVec1Cell=num2cell(tempVec1);
            vectorsForExcelWord1(fMain,gMain+1)=tempVec1Cell(1,gMain);
        end
    end
    if isempty(rowVectorsWord2)==1
        vectorsForExcelWord2{fMain,2}='There is no vector for this word';
    else
        tempVec2=vectors{rowVectorsWord2(1),2};
        for gMain=1:length(tempVec1)
            tempVec2Cell=num2cell(tempVec2);
            vectorsForExcelWord2(fMain,gMain+1)=tempVec2Cell(1,gMain);
        end
    end
end

% save('VectorsWord1.mat','vectorsForExcelWord1')
% save('VectorsWord2.mat','vectorsForExcelWord2')

countOutputCell=1;
%In this loop we create the final cell.
for eMain=1:length(fieldnamesOutputStruct)
    for fMain=1:length(finalOutputStruct.(fieldnamesOutputStruct{eMain})(:,1))
       tempCell{1,:}=finalOutputStruct.(fieldnamesOutputStruct{eMain})(fMain,:);
       outputCell(countOutputCell,:)=tempCell{1}(1,:);
       countOutputCell=countOutputCell+1;
    end
end

%save('ResultsForCrossvalidation.mat','exportVariables')
%save('ResultsLee.mat','outputCell')
xlswrite('ResultsLDT.xls',outputCell)
%save('ResultsPrediciton.mat','outputCell')
end

function [workingMemoryWord1,fieldnamesWorkingMemoryWord1,workingMemoryWord2,fieldnamesWorkingMemoryWord2,lcs]=...
    getVariables(compareWord1,compareWord2,frequencies,germaNet,hierarchy,vectors,zuordnung)
%Now we get the row number resp. the synset number of the input word 1
rowZuordnungCompareWord1 = find(strcmp([zuordnung(:,1)],compareWord1));

%And load the synset number into a variabel, which will then be used to
%name the fields of our temporary struct
for i=1:length(rowZuordnungCompareWord1)
    fieldnamesWorkingMemoryWord1(i)=zuordnung(rowZuordnungCompareWord1(i),2);
end

%Now we get all the lexical Units of the synsets and load them in our
%temporary struct we will work with
for i=1:length(rowZuordnungCompareWord1)
    if length(germaNet.(fieldnamesWorkingMemoryWord1{i}).lexUnit)>1    
        for j=1:length(germaNet.(fieldnamesWorkingMemoryWord1{i}).lexUnit)
            workingMemoryWord1.(fieldnamesWorkingMemoryWord1{i}){j,1}=germaNet.(fieldnamesWorkingMemoryWord1{i}).lexUnit{1,j}.orthForm.Text;
        end
    else
        workingMemoryWord1.(fieldnamesWorkingMemoryWord1{i}){1,1}=germaNet.(fieldnamesWorkingMemoryWord1{i}).lexUnit.orthForm.Text;
    end
end
    
%The same procedure again for the second word
rowZuordnungCompareWord2 = find(strcmp([zuordnung(:,1)],compareWord2));

for i=1:length(rowZuordnungCompareWord2)
    fieldnamesWorkingMemoryWord2(i)=zuordnung(rowZuordnungCompareWord2(i),2);
end

for i=1:length(rowZuordnungCompareWord2)
    if length(germaNet.(fieldnamesWorkingMemoryWord2{i}).lexUnit)>1    
        for j=1:length(germaNet.(fieldnamesWorkingMemoryWord2{i}).lexUnit)
            workingMemoryWord2.(fieldnamesWorkingMemoryWord2{i}){j,1}=germaNet.(fieldnamesWorkingMemoryWord2{i}).lexUnit{1,j}.orthForm.Text;
        end
    else
        workingMemoryWord2.(fieldnamesWorkingMemoryWord2{i}){1,1}=germaNet.(fieldnamesWorkingMemoryWord2{i}).lexUnit.orthForm.Text;
    end
end

%Here we extract the vectors for every lexical Unit of each synset stored
%in our struct for word 1
for i=1:length(fieldnamesWorkingMemoryWord1)
    for j=1:length(workingMemoryWord1.(fieldnamesWorkingMemoryWord1{i}))
        tempWord=compareWord1;
        rowVectorsCompareWord1 = find(strcmp([vectors(:,1)],tempWord));
        if isempty(rowVectorsCompareWord1)==1
            workingMemoryWord1.(fieldnamesWorkingMemoryWord1{i}){j,2}='No vector found for this word';
        else
            workingMemoryWord1.(fieldnamesWorkingMemoryWord1{i}){j,2}=vectors{rowVectorsCompareWord1(1),2};
        end
    end
end

%The same as above for word 2
for i=1:length(fieldnamesWorkingMemoryWord2)
    for j=1:length(workingMemoryWord2.(fieldnamesWorkingMemoryWord2{i}))
        tempWord=compareWord2;
        rowVectorsCompareWord2 = find(strcmp([vectors(:,1)],tempWord));
        if isempty(rowVectorsCompareWord2)==1
            workingMemoryWord2.(fieldnamesWorkingMemoryWord2{i}){j,2}='No vector found for this word';
        else
            workingMemoryWord2.(fieldnamesWorkingMemoryWord2{i}){j,2}=vectors{rowVectorsCompareWord2(1),2};
        end
    end
end

%Now we want to get the freuquencies of every synset we collected so far
for i=1:length(fieldnamesWorkingMemoryWord1)
    rowFrequencyCompareWord1=find(strcmp([frequencies(:,1)],fieldnamesWorkingMemoryWord1{i}));
    workingMemoryWord1.frequencies.(fieldnamesWorkingMemoryWord1{i})(1)=frequencies(rowFrequencyCompareWord1,2);
end

%And for word 2 we want to do the same
for i=1:length(fieldnamesWorkingMemoryWord2)
    rowFrequencyCompareWord2=find(strcmp([frequencies(:,1)],fieldnamesWorkingMemoryWord2{i}));
    workingMemoryWord2.frequencies.(fieldnamesWorkingMemoryWord2{i})(1)=frequencies(rowFrequencyCompareWord2,2);
end

%Now comes the most important part, here we construct the paths from every
%synset to the root of Word 1
for i=1:length(fieldnamesWorkingMemoryWord1)%The loop goes over every word sense stored in germaNet.
    rowHierarchyWord1=find(strcmp([hierarchy(:,2)],fieldnamesWorkingMemoryWord1{i}));%First, we get the row(s) in the hierarchy list of the word sense.    
    countSteps=1;
    countPaths=1;
    if length(rowHierarchyWord1)==1%If the word has only one hypernym, we directly store the hypernym in our path cell.
        pathWord1{i,1}{countPaths,countSteps}=hierarchy{rowHierarchyWord1,2};
    else
        countPaths=length(rowHierarchyWord1);%If there are more than one hypernym, we prepare for the further procedure.
        pathWord1{i,1}{1,countSteps}=hierarchy{rowHierarchyWord1(1),2};        
    end
    while all(cellfun(@isempty,pathWord1{i,1}(:,(countSteps))))==0%If there are more than one way to the top for a lexUnit, we get into this loop.
        if countPaths==1 %The idea of the condition is that when we get to s51001, namely the root, we won't find another hypernym and get an emtpy entry in the path cell. When all paths reached s51001 and get an emtpy entry, we are at the end.
            if length(rowHierarchyWord1)==1%When there is only one hypernym for one of the ways, we directly write it in and continue with the next path resp. next higher level.
                pathWord1{i,1}{1,countSteps+1}=hierarchy{rowHierarchyWord1,3};
                rowHierarchyWord1=find(strcmp([hierarchy(:,2)],pathWord1{i,1}{countPaths,countSteps+1}));
                if isempty(rowHierarchyWord1)%This is the empty entry for the case if we reach s51001.
                    pathWord1{i,1}{1,countSteps+2}=[];
                end
                countPaths=length(rowHierarchyWord1);%And we adjust the number of paths for the upcoming loops.
            end            
        else
            k=1;            
            while isequal(k,(countPaths+1))==0%When we have more than one path, we need to loop every level to collect all the steps of all the paths.               
                rowHierarchyWord1=find(strcmp([hierarchy(:,2)],pathWord1{i,1}{k,countSteps}));                
                if isempty(rowHierarchyWord1)%Again, this is the empty entry for the case if we reach s51001, since from the moment we have more than one path, we stay in the while loop.
                    pathWord1{i,1}{k,(countSteps+1)}=[];
                elseif length(rowHierarchyWord1)==1%If theres only one hypernym in one of the paths, we directly store it.
                    pathWord1{i,1}{k,countSteps+1}=hierarchy{rowHierarchyWord1,3};
                else                    
                    for d=1:length(rowHierarchyWord1)%And if there are more than one hypernym for the next level, we copy the relevant path and ad the next step.
                        if d==1 && length(rowHierarchyWord1)>1
                            for h=1:(length(rowHierarchyWord1)-1)
                                pathWord1{i,1}((k+(1):end+1),:)=pathWord1{i,1}(k:end,:);
                            end
                        end
                        pathWord1{i,1}{k-1+d,countSteps+1}=hierarchy{rowHierarchyWord1(d),3};
                        countPaths=length(pathWord1{i,1}(:,1));%And we adjust the number of paths we need to loop over in the next cycle.                       
                    end
                    k=k+(length(rowHierarchyWord1)-1);                    
                end
                if k<(countPaths+1)
                    k=k+1;
                end
            end           
        end
        countSteps=countSteps+1;
    end
end

%And the same for Word 2
for i=1:length(fieldnamesWorkingMemoryWord2)%The loop goes over every word sense stored in germaNet.
    rowHierarchyWord2=find(strcmp([hierarchy(:,2)],fieldnamesWorkingMemoryWord2{i}));%First, we get the row(s) in the hierarchy list of the word sense.
    countSteps=1;
    countPaths=1;
    if length(rowHierarchyWord2)==1%If the word has only one hypernym, we directly store the hypernym in our path cell.
        pathWord2{i,1}{countPaths,countSteps}=hierarchy{rowHierarchyWord2,2};
    else
        countPaths=length(rowHierarchyWord2);%If there are more than one hypernym, we prepare for the further procedure.
        pathWord2{i,1}{1,countSteps}=hierarchy{rowHierarchyWord2(1),2};        
    end
    while all(cellfun(@isempty,pathWord2{i,1}(:,(countSteps))))==0%If there are more than one way to the top for a lexUnit, we get into this loop.
        if countPaths==1%The idea of the condition is that when we get to s51001, namely the root, we won't find another hypernym and get an emtpy entry in the path cell. When all paths reached s51001 and get an emtpy entry, we are at the end.
            if length(rowHierarchyWord2)==1%When there is only one hypernym for one of the ways, we directly write it in and continue with the next path resp. next higher level.
                pathWord2{i,1}{1,countSteps+1}=hierarchy{rowHierarchyWord2,3};
                rowHierarchyWord2=find(strcmp([hierarchy(:,2)],pathWord2{i,1}{countPaths,countSteps+1}));
                if isempty(rowHierarchyWord2)%This is the empty entry for the case if we reach s51001.
                    pathWord2{i,1}{1,countSteps+2}=[];
                end
                countPaths=length(rowHierarchyWord2);%And we adjust the number of paths for the upcoming loops.
            end            
        else
            k=1;            
            while isequal(k,(countPaths+1))==0%When we have more than one path, we need to loop every level to collect all the steps of all the paths.                
                rowHierarchyWord2=find(strcmp([hierarchy(:,2)],pathWord2{i,1}{k,countSteps}));                
                if isempty(rowHierarchyWord2)%Again, this is the empty entry for the case if we reach s51001, since from the moment we have more than one path, we stay in the while loop.
                    pathWord2{i,1}{k,(countSteps+1)}=[];
                elseif length(rowHierarchyWord2)==1%If there's only one hypernym in one of the paths, we directly store it.
                    pathWord2{i,1}{k,countSteps+1}=hierarchy{rowHierarchyWord2,3};
                else                    
                    for d=1:length(rowHierarchyWord2)%And if there are more than one hypernym for the next level, we copy the relevant path and ad the next step.
                        if d==1 && length(rowHierarchyWord2)>1
                            for h=1:(length(rowHierarchyWord2)-1)
                                pathWord2{i,1}((k+(1):end+1),:)=pathWord2{i,1}(k:end,:);
                            end
                        end
                        pathWord2{i,1}{k-1+d,countSteps+1}=hierarchy{rowHierarchyWord2(d),3};
                        countPaths=length(pathWord2{i,1}(:,1));%And we adjust the number of paths we need to loop over in the next cycle.                       
                    end
                    k=k+(length(rowHierarchyWord2)-1);                    
                end
                if k<(countPaths+1)
                    k=k+1;
                end
            end           
        end
        countSteps=countSteps+1;
    end
end

%Now we extract one of the most important variables for the calculations we
%will be doing later: the lowest common subsumber (lcs). This seems
%complicated, and in the beginning it was.
countLCS=1;
countSynsets=1;
for e=1:length(pathWord1)%Loop over every lexUnit of word 1.
    for f=1:length(pathWord2)%Loop over every lexUnit of word 2.
        for c=1:length(pathWord1{e,1}(:,1))%Loop over every path of every lexUnit of word 1.
            for d=1:length(pathWord2{f,1}(:,1))%Loop over every path of every lexUnit of word 2.   
                for a=1:length(pathWord1{e,1})%Loop over the whole length of the chosen path of the lexUnit of word 1.
                    for b=1:length(pathWord2{f,1})%Loop over the whole length of the chosen path of the lexUnit of word 2. 
                        if isempty(pathWord1{e,1}(c,a))==0
                            if strcmp(pathWord1{e,1}(c,a),pathWord2{f,1}(d,b))==1%If we find the lcs, it gets written down.
                                if countLCS==1 %This conditions allows only the first lcs found to be stored, since the higher level ones we don't need.
                                    %To have all the necessery
                                    %information, we store...
                                    pathLength=a+b-2;
                                    lcsTemp{countSynsets}{c,d}{1,1}=pathWord1{e,1}{c,a};%the lcs itself,
                                    lcsTemp{countSynsets}{c,d}{1,2}=pathLength;%the pathlength between the two words,                                    
                                    lcsTemp{countSynsets}{c,d}{1,3}=pathWord1{e,1}{1,1};%the lexUnit of word 1,
                                    lcsTemp{countSynsets}{c,d}{1,4}=pathWord2{f,1}{1,1};%and the lexUnit of word 2.
                                    countLCS=countLCS+1;
                                end
                            end
                        end                
                    end            
                end
                countLCS=1;%If we finished one lexUnit of word 1, we search the lcs's for the other lexUnits.
            end
        end
        countSynsets=countSynsets+1;%And now the next synset of word 2.
    end
end

%Since there is the possibility that two words have multiple ways that
%connect them and that they are equally long, we take the subsumer that is
%deeper in the hierarchy, meaning we're taking the lcs which has a longer
%way to the root.
for a=1:length(lcsTemp)
    for y=1:length(lcsTemp{a}(1,:))
        for x=1:length(lcsTemp{a}(:,1))
            pathLengths{a}(x,y)=lcsTemp{a}{x,y}{1,2};
        end
    end
end

%To do so, we need to find the ways of the lcs's to the root as we did for
%the words themselves.
for a=1:length(pathLengths)
    minPathLengthTemp = min(pathLengths{1,a}(:,:));
    minPathLength=min(minPathLengthTemp);
    [row,col] = find(pathLengths{1,a}==minPathLength);
    if length(row)>1&&strcmp(lcsTemp{1,a}{1,1}{1,3},lcsTemp{1,a}{1,1}{1,4})==0%If we find more than one lcs with minimal
        %path length between the two synsets, we begin the procedure of finding the deepest one.
        for x=1:length(row)%First, we extract the lexUnits of the lcs's.
            fieldnamesLCS{x}=lcsTemp{1,a}{row(x),col(x)}(1,1);
        end
        %From here on, the process of the path construction is the same as
        %above.
        for i=1:length(fieldnamesLCS)
            rowHierarchyLCS=find(strcmp([hierarchy(:,2)],fieldnamesLCS{i}));    
            countSteps=1;
            countPaths=1;
            if isempty(rowHierarchyLCS)==1
                pathLCS{i,1}{countPaths,countSteps}=fieldnamesLCS(i);
                continue
            end
            if length(rowHierarchyLCS)==1
                pathLCS{i,1}{countPaths,countSteps}=hierarchy{rowHierarchyLCS,2};
            else
                countPaths=length(rowHierarchyLCS);
                pathLCS{i,1}{countPaths,countSteps}=hierarchy{rowHierarchyLCS(1),2};        
            end
            while all(cellfun(@isempty,pathLCS{i,1}(:,(countSteps))))==0
                if countPaths==1
                    if length(rowHierarchyLCS)==1
                        pathLCS{i,1}{1,countSteps+1}=hierarchy{rowHierarchyLCS,3};
                        rowHierarchyLCS=find(strcmp([hierarchy(:,2)],pathLCS{i,1}{countPaths,countSteps+1}));
                        if isempty(rowHierarchyLCS)
                            pathLCS{i,1}{1,countSteps+2}=[];
                        end
                        countPaths=length(rowHierarchyLCS);
                    end            
                else
                    k=1;            
                    while isequal(k,(countPaths+1))==0                
                        rowHierarchyLCS=find(strcmp([hierarchy(:,2)],pathLCS{i,1}{k,countSteps}));                
                        if isempty(rowHierarchyLCS)
                            pathLCS{i,1}{k,(countSteps+1)}=[];
                        elseif length(rowHierarchyWord2)==1
                            pathLCS{i,1}{k,countSteps+1}=hierarchy{rowHierarchyLCS,3};
                        else                    
                            for d=1:length(rowHierarchyLCS)
                                if d==1 && length(rowHierarchyLCS)>1
                                    for h=1:(length(rowHierarchyLCS)-1)
                                        pathLCS{i,1}((k+(1):end+1),:)=pathLCS{i,1}(k:end,:);
                                    end
                                end
                                pathLCS{i,1}{k-1+d,countSteps+1}=hierarchy{rowHierarchyLCS(d),3};
                                countPaths=length(pathLCS{i,1}(:,1));                        
                            end
                            k=k+(length(rowHierarchyLCS)-1);                     
                        end
                        if k<(countPaths+1)
                           k=k+1;
                        end
                    end           
                end
                countSteps=countSteps+1;
            end
        end        
        %Now comes the part where we compare the paths and choose the
        %longest one since the longer the way, the deeper resp. lower is
        %the lcs.
        for b=1:length(pathLCS) 
            for c=1:length(pathLCS(b))
                tempCount=pathLCS{b}(c,:);
                tempCount=tempCount(~cellfun('isempty',tempCount));
                lengthLCS{b}(c)=length(tempCount);
            end
            tempLengthLCS=lengthLCS{b}(:);
            maxPathLengthLCS= max(tempLengthLCS);
            [rowLCS,colLCS] = find(tempLengthLCS==maxPathLengthLCS);
            realLCS(b)=lengthLCS{b}(rowLCS,colLCS);
        end
        maxPathLength = max(realLCS(:));
        [rowFinal,colFinal] = find(realLCS==maxPathLength);
        lcsRaw{a}=lcsTemp{1,a}(row(rowFinal),col(rowFinal));
    else
        if strcmp(lcsTemp{1,a}{1,1}{1,3},lcsTemp{1,a}{1,1}{1,4})==0
            lcsRaw{a}=lcsTemp{1,a}(row,col);
        else
            lcsRaw{a}=lcsTemp{1,a}(1,1);
        end
    end
end

%To make the data look nicer, we unpack it one level 
for a=1:length(lcsRaw)
    for b=1:length(lcsRaw{1,a})
        lcs(a,b)=lcsRaw{1,a}(1,b);
    end
end

%We nearly did it! The second last variable is the frequencies of the
%lcs's, which we now ad to the lcs-cell.
for a=1:length(lcs(:,1))
    rowFrequencyLCS=find(strcmp([frequencies(:,1)],lcs{a}{1,1}));
    lcs{a}(1,5)=frequencies(rowFrequencyLCS,2);
    rowZuordnungLCS=find(strcmp([zuordnung(:,2)],lcs{a}{1,1}));
    lcs{a}{1,7}=zuordnung(rowZuordnungLCS,1);
    rowRoot=find(strcmp([frequencies(:,1)],'s51001'));
    lcs{a}{1,8}=frequencies(rowRoot,2);
end

%And now the last variable we need for the calculations is the length of
%the way of the lcs to the root. This is the point where I didn't pay
%attention to the principle of no repetitions, since this step is redundant
%and could have been included in the loop above, but I noticed too late and
%was afraid of errors and too lazy to shorten the whole thing ;).
for i=1:length(lcs(:,1))
    rowHierarchyLCS=find(strcmp([hierarchy(:,2)],lcs{i}(1,1)));    
    countSteps=1;
    countPaths=1;
    if isempty(rowHierarchyLCS)==1
        lcsPaths{i,1}{countPaths,countSteps}=lcs{i}{1,1};
        continue
    end
    if length(rowHierarchyLCS)==1
        lcsPaths{i,1}{countPaths,countSteps}=hierarchy{rowHierarchyLCS,2};
    else
        countPaths=length(rowHierarchyLCS);
        lcsPaths{i,1}{1,countSteps}=hierarchy{rowHierarchyLCS(1),2};        
    end
    while all(cellfun(@isempty,lcsPaths{i,1}(:,(countSteps))))==0
        if countPaths==1
            if length(rowHierarchyLCS)==1
                lcsPaths{i,1}{1,countSteps+1}=hierarchy{rowHierarchyLCS,3};
                rowHierarchyLCS=find(strcmp([hierarchy(:,2)],lcsPaths{i,1}{countPaths,countSteps+1}));
                if isempty(rowHierarchyLCS)
                    lcsPaths{i,1}{1,countSteps+2}=[];
                end
                countPaths=length(rowHierarchyLCS);
            end            
        else
            k=1;            
            while isequal(k,(countPaths+1))==0                
                rowHierarchyLCS=find(strcmp([hierarchy(:,2)],lcsPaths{i,1}{k,countSteps}));                
                if isempty(rowHierarchyLCS)
                    lcsPaths{i,1}{k,(countSteps+1)}=[];
                elseif length(rowHierarchyLCS)==1
                    lcsPaths{i,1}{k,countSteps+1}=hierarchy{rowHierarchyLCS,3};
                else                    
                    for d=1:length(rowHierarchyLCS)
                        if d==1 && length(rowHierarchyLCS)>1
                            for h=1:(length(rowHierarchyLCS)-1)
                                lcsPaths{i,1}((k+(1):end+1),:)=lcsPaths{i,1}(k:end,:);
                            end
                        end
                        lcsPaths{i,1}{k-1+d,countSteps+1}=hierarchy{rowHierarchyLCS(d),3};
                        countPaths=length(lcsPaths{i,1}(:,1));                        
                    end
                    k=k+(length(rowHierarchyLCS)-1);                    
                end
                if k<(countPaths+1)
                   k=k+1;
                end
            end           
        end
        countSteps=countSteps+1;
    end
end

%We look for the longest path, measure its length and store it to the lcs
%cell.
for b=1:length(lcsPaths) 
    if length(lcsPaths(b))>1%If there are multiple ways, we compare them.
        for c=1:length(lcsPaths(b))
            tempCount=lcsPaths{b}(c,:);
            tempCount=tempCount(~cellfun('isempty',tempCount));
            tempLCSPaths(c)=length(tempCount)-1;                       
        end
        maxPathLengthLCS = max(tempLCSPaths(:));
        [rowLCS,colLCS] = find(tempLCSPaths==maxPathLengthLCS);
        lcs{b}{1,6}=tempLCSPaths(rowLCS,colLCS); 
    else%If there is only one way, we measure it and store the length into the lcs cell.
        tempCount=lcsPaths{b}(1,:);
        tempCount=tempCount(~cellfun('isempty',tempCount));
        lcs{b}{1,6}=length(tempCount)-1;        
    end
end
end

function [ tempFinalOutput ] = comparsionGermaNetW2V(compareWord1,compareWord2,compareLCS)
    tempFinalOutput{1,1}='Word 1';
    tempFinalOutput{1,2}='lexUnit';
    tempFinalOutput{1,3}='Word 2';
    tempFinalOutput{1,4}='lexUnit';
    tempFinalOutput{1,5}='Word LCS';
    tempFinalOutput{1,6}='lexUnit';
    tempFinalOutput{1,7}='Path Word 1 to Word 2';
    tempFinalOutput{1,8}='Path to root';
    tempFinalOutput{2,1}=compareWord1{1};
    tempFinalOutput{2,2}=compareWord1{2};
    tempFinalOutput{2,3}=compareWord2{1};
    tempFinalOutput{2,4}=compareWord2{2};
    if length(compareLCS{7})>1
        firstLCS=compareLCS{7}(1,1);
        tempFinalOutput(2,5)=firstLCS;
    else
        tempFinalOutput(2,5)=compareLCS{7}(1,1);
    end
    tempFinalOutput{2,6}=compareLCS{1};
    tempFinalOutput{2,7}=compareLCS{2};
    tempFinalOutput{2,8}=compareLCS{6};
    pathBetween(1,:)=compareLCS{1,2}(1);
    freqLCS(1)=compareLCS{1,5}(1);
    pathRoot(1)=compareLCS{1,6}(1);
    freqRoot(1)=compareLCS{1,8}{1,1}(1);
    freqW1(1)=compareWord1{1,3}{1,1}(1);
    freqW2(1)=compareWord2{1,3}{1,1}(1);
    vectorW1(1,:)=compareWord1{4}(1,:);
    vectorW2(1,:)=compareWord2{4}(1,:);
    resnikResult=(-1*(log(freqLCS/freqRoot)));
    tempFinalOutput{1,9}='Resnik';
    tempFinalOutput{2,9}=resnikResult;
    linResult=(2*log(freqLCS/freqRoot))/(log(freqW1/freqRoot)+log(freqW2/freqRoot));
    tempFinalOutput{1,10}='Lin';
    tempFinalOutput{2,10}=linResult;
    liResult=exp(-1*(0.2*pathBetween))*((exp(0.6*pathRoot)-exp(-0.6*pathRoot))/(exp(0.6*pathRoot)+exp(-0.6*pathRoot)));
    tempFinalOutput{1,11}='Li, Bandar & McLean';
    tempFinalOutput{2,11}=liResult;
    cosineDistance=dot(vectorW1,vectorW2)/(norm(vectorW1)*norm(vectorW2));
    tempFinalOutput{1,12}='Cosine Distance';
    tempFinalOutput{2,12}=cosineDistance;
    if compareLCS{2}==2 && strcmp(compareLCS{1},compareWord1{2})==0 && strcmp(compareLCS{1},compareWord2{2})==0
        iacobacci=(dot(vectorW1,vectorW2)/((norm(vectorW1)^2)+(norm(vectorW2)^2)-dot(vectorW1,vectorW2)))*1.6;
    else
        iacobacci=(dot(vectorW1,vectorW2)/((norm(vectorW1)^2)+(norm(vectorW2)^2)-dot(vectorW1,vectorW2)))*(1.6^(-1));
    end
    tempFinalOutput{1,13}='Iacobacci';
    tempFinalOutput{2,13}=iacobacci;
    if compareLCS{2}==2 && strcmp(compareLCS{1},compareWord1{2})==0 && strcmp(compareLCS{1},compareWord2{2})==0
        iacobacciLi=(0.65*(((dot(vectorW1,vectorW2)/((norm(vectorW1)^2)+(norm(vectorW2)^2)-dot(vectorW1,vectorW2)))*1.6)))...
            +(0.35*((exp(-1*(0.2*pathBetween))*((exp(0.6*pathRoot)-exp(-0.6*pathRoot))/(exp(0.6*pathRoot)+exp(-0.6*pathRoot))))));
    else
        iacobacciLi=(0.65*((dot(vectorW1,vectorW2)/((norm(vectorW1)^2)+(norm(vectorW2)^2)-dot(vectorW1,vectorW2)))*(1.6^(-1))))...
            +(0.35*(exp(-1*(0.2*pathBetween))*((exp(0.6*pathRoot)-exp(-0.6*pathRoot))/(exp(0.6*pathRoot)+exp(-0.6*pathRoot)))));
    end
    tempFinalOutput{1,14}='Iacobacci and Li';
    tempFinalOutput{2,14}=iacobacciLi;
    if pathBetween==0
        cosineLi=(0.65*((dot(vectorW1,vectorW2)/(norm(vectorW1)*norm(vectorW2)))*(exp(-1*(0.2*1))))+...
            (0.35*(((exp(0.6*pathRoot)-exp(-0.6*pathRoot))/(exp(0.6*pathRoot)+exp(-0.6*pathRoot))))));
    else
        cosineLi=(0.65*((dot(vectorW1,vectorW2)/(norm(vectorW1)*norm(vectorW2)))*(exp(-1*(0.2*pathBetween))))+...
            (0.35*(((exp(0.6*pathRoot)-exp(-0.6*pathRoot))/(exp(0.6*pathRoot)+exp(-0.6*pathRoot))))));
    end
    tempFinalOutput{1,15}='Cosine and Li';
    tempFinalOutput{2,15}=cosineLi;
    if pathBetween==0
        leeResult=(0.55*(dot(vectorW1,vectorW2)/(norm(vectorW1)*norm(vectorW2)))+((0.45*(1/1))));
    else
        leeResult=(0.55*(dot(vectorW1,vectorW2)/(norm(vectorW1)*norm(vectorW2)))+((0.45*(1/pathBetween))));
    end
    tempFinalOutput{1,16}='Lee';
    tempFinalOutput{2,16}=leeResult;
    if pathBetween==0
        iacobacciPath=(dot(vectorW1,vectorW2)/((norm(vectorW1)^2)+(norm(vectorW2)^2)-dot(vectorW1,vectorW2)))*(1/1);
    else
        iacobacciPath=(dot(vectorW1,vectorW2)/((norm(vectorW1)^2)+(norm(vectorW2)^2)-dot(vectorW1,vectorW2)))*(1/pathBetween);
    end
    tempFinalOutput{1,17}='Iacobacci and Path';
    tempFinalOutput{2,17}=iacobacciPath;
    tempFinalOutput{1,18}='Path Between';
    tempPath1=compareLCS{2};
    tempFinalOutput{2,18}=tempPath1;
    tempFinalOutput{1,19}='Path Root';
    tempPath2=compareLCS{6};
    tempFinalOutput{2,19}=tempPath2;
    tempFinalOutput{1,20}='Sense Comb';
    sensesWord1=length(fieldnamesWorkingMemoryWord1);
    sensesWord2=length(fieldnamesWorkingMemoryWord2);
    comparsionsSenses=sensesWord1*sensesWord2;
    tempFinalOutput{2,20}=comparsionsSenses;
end

function [ tempFinalOutput ] = comparsionGermaNet(compareWord1,compareWord2,compareLCS)
    tempFinalOutput{1,1}='Word 1';
    tempFinalOutput{1,2}='lexUnit';
    tempFinalOutput{1,3}='Word 2';
    tempFinalOutput{1,4}='lexUnit';
    tempFinalOutput{1,5}='Word LCS';
    tempFinalOutput{1,6}='lexUnit';
    tempFinalOutput{1,7}='Path Word 1 to Word 2';
    tempFinalOutput{1,8}='Path to root';
    tempFinalOutput{2,1}=compareWord1{1};
    tempFinalOutput{2,2}=compareWord1{2};
    tempFinalOutput{2,3}=compareWord2{1};
    tempFinalOutput{2,4}=compareWord2{2};
    if length(compareLCS{1}{7})>1
        firstLCS=compareLCS{1}{7}(1,1);
        tempFinalOutput(2,5)=firstLCS;
    else
        tempFinalOutput(2,5)=compareLCS{1}{7}(1,1);
    end
    tempFinalOutput{2,6}=compareLCS{1}{1};
    tempFinalOutput{2,7}=compareLCS{1}{2};
    tempFinalOutput{2,8}=compareLCS{1}{6};
    pathBetween(1,:)=compareLCS{1}{1,2}(1);
    freqLCS(1)=compareLCS{1}{1,5}(1);
    pathRoot(1)=compareLCS{1}{1,6}(1);
    freqRoot(1)=compareLCS{1}{1,8}{1,1}(1);
    freqW1(1)=compareWord1{1,3}{1,1}(1);
    freqW2(1)=compareWord2{1,3}{1,1}(1);
    vectorW1(1,:)=compareWord1{4}(1,:);
    vectorW2(1,:)=compareWord2{4}(1,:);
    resnikResult=(-1*(log(freqLCS/freqRoot)));
    tempFinalOutput{1,9}='Resnik';
    tempFinalOutput{2,9}=resnikResult;
    linResult=(2*log(freqLCS/freqRoot))/(log(freqW1/freqRoot)+log(freqW2/freqRoot));
    tempFinalOutput{1,10}='Lin';
    tempFinalOutput{2,10}=linResult;
    liResult=exp(-1*(0.2*pathBetween))*((exp(0.6*pathRoot)-exp(-0.6*pathRoot))/(exp(0.6*pathRoot)+exp(-0.6*pathRoot)));
    tempFinalOutput{1,11}='Li, Bandar & McLean';
    tempFinalOutput{2,11}=liResult;
    tempFinalOutput{1,12}='Cosine Distance';
    tempFinalOutput{2,12}='NaN';
    tempFinalOutput{1,13}='Iacobacci';
    tempFinalOutput{2,13}='NaN';
    tempFinalOutput{1,14}='Iacobacci and Li';
    tempFinalOutput{2,14}='NaN';
    tempFinalOutput{1,15}='Cosine and Li';
    tempFinalOutput{2,15}='NaN';
    tempFinalOutput{1,16}='Lee';
    tempFinalOutput{2,16}='NaN';
    tempFinalOutput{1,17}='Iacobacci and Path';
    tempFinalOutput{2,17}='NaN';
    tempFinalOutput{1,18}='Frequencie W1';
    tempFreqW1=freqW1;%/freqRoot;
    tempFinalOutput{2,18}=tempFreqW1;
    tempFinalOutput{1,19}='Frequencie W2';
    tempFreqW2=freqW2;%/freqRoot;
    tempFinalOutput{2,19}=tempFreqW2;
end

function [ out ] = predictionGermaNetW2V(predictWord,similarity)

end

function [ out ] = predictionGermaNet(predictWord,similarity)

end

function [ out ] = predictionW2V(predictWord,similarity)

end
