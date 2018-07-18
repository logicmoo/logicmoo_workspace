void matrixSetAll(int NRows, int NColumns, double array[],double e){
    int index,i,j;
    for(i=0;i<NRows;i++){
        for(j=0;j<NColumns;j++){
            index=i*NColumns+j;
            array[index]=e;
        }
    }
}

void matrixEye(int NRows, int NColumns, double array[]){
    int index,i,j;
    for(i=0;i<NRows;i++){
        for(j=0;j<NColumns;j++){
            index=i*NColumns+j;
            if(i==j)
                array[index]=1;
            else
                array[index]=0;
        }
    }
}
