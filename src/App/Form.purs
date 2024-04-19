module App.Form (component) where

import Prelude

import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import JS.BigInt (BigInt)
import JS.BigInt as BigInt

type Form :: (Type -> Type -> Type -> Type) -> Row Type
type Form f =
  ( factorA :: f String String BigInt
  , factorB :: f String String BigInt
  , product :: f String String BigInt
  )

type FormContext = F.FormContext (Form F.FieldState) (Form (F.FieldAction Action)) Unit Action
type FormlessAction = F.FormlessAction (Form F.FieldState)

data Action
  = Receive FormContext
  | Eval FormlessAction

component :: forall query. H.Component query Unit { | Form F.FieldOutput } Aff
component = F.formless { liftAction: Eval } mempty $ H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval $ H.defaultEval
      { receive = Just <<< Receive
      , handleAction = handleAction
      , handleQuery = handleQuery
      }
  }
  where
  handleAction :: Action -> H.HalogenM _ _ _ _ _ Unit
  handleAction = case _ of
    Receive context -> H.put context
    Eval action -> F.eval action

  handleQuery :: forall a. F.FormQuery _ _ _ _ a -> H.HalogenM _ _ _ _ _ (Maybe a)
  handleQuery = do
    let
      validation :: { | Form F.FieldValidation }
      validation =
        let
          parseInt s
            | s == "" = Left "Required"
            | otherwise = note "Invalid Int" $ BigInt.fromString s

        in
          { factorA: parseInt
          , factorB: parseInt
          , product: parseInt
          }

    F.handleSubmitValidate F.raise F.validate validation

  render :: FormContext -> H.ComponentHTML Action () Aff
  render { formActions, fields, actions } =
    HH.form
      [ HE.onSubmit formActions.handleSubmit ]
      [ HH.div_
          [ HH.text "Enter numbers a and b which mulitply to n."
          ]
      , HH.div_
          [ HH.label_ [ HH.text "a: " ]
          , HH.input
              [ HP.type_ HP.InputText
              , HE.onValueInput actions.factorA.handleChange
              , HE.onBlur actions.factorA.handleBlur
              , case fields.factorA.result of
                  Nothing -> HP.placeholder "5"
                  Just (Left _) -> HP.attr (HH.AttrName "aria-invalid") "true"
                  Just (Right _) -> HP.attr (HH.AttrName "aria-invalid") "false"
              ]
          , case fields.factorA.result of
              Just (Left err) -> HH.small_ [ HH.text err ]
              _ -> HH.text ""
          ]
      , HH.div_
          [ HH.label_ [ HH.text "b: " ]
          , HH.input
              [ HP.type_ HP.InputText
              , HE.onValueInput actions.factorB.handleChange
              , HE.onBlur actions.factorB.handleBlur
              , case fields.factorB.result of
                  Nothing -> HP.placeholder "2"
                  Just (Left _) -> HP.attr (HH.AttrName "aria-invalid") "true"
                  Just (Right _) -> HP.attr (HH.AttrName "aria-invalid") "false"
              ]
          , case fields.factorB.result of
              Just (Left err) -> HH.small_ [ HH.text err ]
              _ -> HH.text ""
          ]
      , HH.div_
          [ HH.label_ [ HH.text "n: " ]
          , HH.input
              [ HP.type_ HP.InputText
              , HE.onValueInput actions.product.handleChange
              , HE.onBlur actions.product.handleBlur
              , case fields.product.result of
                  Nothing -> HP.placeholder "10"
                  Just (Left _) -> HP.attr (HH.AttrName "aria-invalid") "true"
                  Just (Right _) -> HP.attr (HH.AttrName "aria-invalid") "false"
              ]
          , case fields.product.result of
              Just (Left err) -> HH.small_ [ HH.text err ]
              _ -> HH.text ""
          ]
      , HH.button
          [ HP.type_ HP.ButtonSubmit ]
          [ HH.text "Submit" ]
      ]