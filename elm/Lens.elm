module Lens
    exposing
        ( Lens
        , PureLens
        , get
        , goIn
        , update
        , updateIf
        )


type alias Lens submodel updatedSubmodel model updatedModel =
    { get : model -> Maybe submodel
    , set : updatedSubmodel -> model -> Maybe updatedModel
    }


type alias PureLens submodel model =
    Lens submodel submodel model model


get :
    Lens submodel updatedSubmodel model updatedModel
    -> model
    -> Maybe submodel
get { get } =
    get


goIn :
    Lens submodel updatedSubmodel model updatedModel
    -> Lens model updatedModel supermodel updatedSupermodel
    -> Lens submodel updatedSubmodel supermodel updatedSupermodel
goIn submodelInModel modelInSupermodel =
    { get = modelInSupermodel.get >> Maybe.andThen submodelInModel.get
    , set =
        \submodel supermodel ->
            modelInSupermodel.get supermodel
                |> Maybe.andThen
                    (submodelInModel.set submodel
                        >> Maybe.andThen
                            (flip modelInSupermodel.set supermodel)
                    )
    }


update :
    Lens submodel updatedSubmodel model updatedModel
    -> (submodel -> updatedSubmodel)
    -> model
    -> Maybe updatedModel
update { get, set } upd model =
    get model |> Maybe.andThen (upd >> flip set model)


updateIf :
    Lens submodel updatedSubmodel model updatedModel
    -> (submodel -> model -> updatedModel)
    -> model
    -> Maybe updatedModel
updateIf { get } upd model =
    get model |> Maybe.map (flip upd model)
