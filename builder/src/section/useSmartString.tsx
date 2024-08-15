import { createRef, RefObject, Component } from "preact";

import { SmartStringCondition, SmartString } from "../types";
import { Children } from "preact/compat";
import { h, FunctionalComponent, ComponentChildren, VNode } from "preact";

function standardizeSmartStringInput(input: SmartStringCondition[]): SmartStringCondition[] {
  return input.map((item) => {
    if (typeof item === "string") {
      return { en: item };
    }
    return item;
  });
}

type SmartStringInputProps = {
  id?: string;
  class?: string;
  value?: SmartString | undefined;
  onChange?: (event: Event) => void;
  onKeyUp?: (event: KeyboardEvent) => void | undefined;
  children?: any;
  htmlFor?: string;
  typeId?: string;
};

export const SmartStringInputDeprecated: FunctionalComponent<SmartStringInputProps> = (props) => {
  return null; // Explicitly render nothing. It is used only for extracting props.
};

type SmartStringTextAreaProps = {
  id?: string;
  class?: string;
  value?: SmartString | undefined;
  rows?: number;
  onChange?: (event: Event) => void;
  onKeyUp?: (event: KeyboardEvent) => void | undefined;
  children?: any;
  htmlFor?: string;
  typeId?: string;
};

export const SmartStringTextArea: FunctionalComponent<SmartStringTextAreaProps> = (props) => {
  return null; // Explicitly render nothing. It is used only for extracting props.
};

type SmartStringDivProps = {
  children: ComponentChildren;
  style?: h.JSX.CSSProperties;
  clazz?: string;
  divRef?: RefObject<HTMLDivElement>;
};

interface SmartStringDivState {
  inputRefs: RefObject<HTMLInputElement | HTMLTextAreaElement>[];
  inputProps: SmartStringInputProps;
  textAreaProps: SmartStringTextAreaProps;
  smartString: SmartString;
  hasRenderedOnce: boolean;
}

export class SmartStringDiv extends Component<SmartStringDivProps, SmartStringDivState> {
  state: {
    inputRefs: RefObject<HTMLInputElement | HTMLTextAreaElement>[];
    inputProps: SmartStringInputProps;
    textAreaProps: SmartStringTextAreaProps;
    smartString: SmartString;
    hasRenderedOnce: boolean;
  };

  constructor(props: SmartStringDivProps) {
    super(props);
    this.state = {
      inputRefs: [],
      inputProps: {},
      textAreaProps: {},
      smartString: "",
      hasRenderedOnce: false,
    };
  }

  componentDidMount() {
    this.extractPropsFromChildren();
    this.updateInputRefsBasedOnSmartString();
  }

  shouldComponentUpdate(nextProps: SmartStringDivProps, nextState: SmartStringDivState) {
    if (JSON.stringify(this.props.style) !== JSON.stringify(nextProps.style)) {
      return true;
    }
    // return true
    return !this.state.hasRenderedOnce;
  }

  componentDidUpdate(prevProps: SmartStringDivProps, prevState: SmartStringDivState) {
    if (prevState.smartString !== this.state.smartString) {
      this.updateInputRefsBasedOnSmartString();
    }
    if (!this.state.hasRenderedOnce) {
      this.setState({ hasRenderedOnce: true });
    }
  }

  extractPropsFromChildren() {
    Children.toArray(this.props.children).forEach((child: VNode<any>) => {
      if (child && typeof child.type === "function") {
        const typeName = child.props.typeId;
        if (typeName === "SmartStringTextArea") {
          const newTextAreaProps = child.props;
          this.setState((prevState) => ({
            textAreaProps: newTextAreaProps,
            smartString: newTextAreaProps.value === undefined ? "" : newTextAreaProps.value || prevState.smartString,
          }));
        } else {
          const newInputProps = child.props;
          this.setState((prevState) => ({
            inputProps: newInputProps,
            smartString: newInputProps.value === undefined ? "" : newInputProps.value || prevState.smartString,
          }));
        }
      }
    });
  }

  updateInputRefsBasedOnSmartString() {
    const { smartString } = this.state;
    if (Array.isArray(smartString)) {
      this.setState({
        inputRefs: Array(smartString.length * 2)
          .fill(null)
          .map((_, i) => this.state.inputRefs[i] || createRef()),
      });
    } else {
      this.setState({ inputRefs: [createRef()] });
    }
  }

  getCurrentSmartString(): SmartString {
    const { smartString, inputRefs } = this.state;
    if (typeof smartString === "string") {
      return inputRefs[0]?.current ? inputRefs[0].current.value : smartString;
    } else if (Array.isArray(smartString)) {
      return standardizeSmartStringInput(smartString).map((item, index) => {
        const enInput = inputRefs[index * 2]?.current;
        const includeIfInput = inputRefs[index * 2 + 1]?.current;

        const newSmartStringCondition: SmartStringCondition = {
          en: enInput ? enInput.value : item.en,
        };

        if (item.includeIf !== undefined) {
          newSmartStringCondition.includeIf = includeIfInput ? includeIfInput.value : item.includeIf;
        }

        return newSmartStringCondition;
      });
    }
    return "";
  }

  get value() {
    return this.getCurrentSmartString();
  }

  checkVisibility = (): boolean => {
    return this.state.inputRefs.every((ref) => {
      if (ref && ref.current) {
        return ref.current.checkVisibility();
      }
      return true;
    });
  };

  render() {
    const { style, clazz, divRef } = this.props;
    const { inputProps, smartString, inputRefs, textAreaProps } = this.state;
    const isInputEmpty = inputProps && Object.keys(inputProps).length === 0;
    const isTextAreaEmpty = textAreaProps && Object.keys(textAreaProps).length === 0;

    let refIndex = 0;
    let labelChild = inputProps.children ? inputProps.children : textAreaProps.children;
    let labelFor = inputProps.id || inputProps.id != undefined ? inputProps.id : textAreaProps.id;
    let labelHtmlFor = inputProps.htmlFor ? inputProps.htmlFor : textAreaProps.htmlFor;
    return (
      <div class={clazz} style={style} ref={divRef}>
        {typeof smartString === "string" ? (
          <>
            <label for={labelFor} htmlFor={labelHtmlFor}>
              {labelChild}
            </label>
            {isInputEmpty ? null : (
              <input
                id={inputProps.id}
                class={inputProps.class}
                value={smartString as string}
                ref={inputRefs[0] as RefObject<HTMLInputElement>}
                onChange={inputProps.onChange}
                onKeyUp={inputProps.onKeyUp}
              />
            )}
            {isTextAreaEmpty ? null : (
              <textarea
                id={textAreaProps.id}
                class={textAreaProps.class}
                rows={textAreaProps.rows}
                ref={inputRefs[0] as RefObject<HTMLTextAreaElement>}
                onKeyUp={textAreaProps.onKeyUp}
              >
                {smartString as string}
              </textarea>
            )}
          </>
        ) : Array.isArray(smartString) ? (
          (smartString as SmartStringCondition[]).map((item, index) => (
            <>
              {isInputEmpty ? null : (
                <>
                  <label for={inputProps.id + "-" + index}>{labelChild}</label>
                  <input
                    id={inputProps.id + "-" + index}
                    class={inputProps.class}
                    value={typeof item === "object" ? item.en : item}
                    ref={inputRefs[refIndex++] as RefObject<HTMLInputElement>}
                    onChange={inputProps.onChange}
                    onKeyUp={inputProps.onKeyUp}
                  />
                </>
              )}

              {isTextAreaEmpty ? null : (
                <>
                  <label for={textAreaProps.id + "-" + index}>{labelChild}</label>
                  <textarea
                    id={textAreaProps.id + "-" + index}
                    class={textAreaProps.class}
                    rows={textAreaProps.rows}
                    ref={inputRefs[refIndex++] as RefObject<HTMLTextAreaElement>}
                    onKeyUp={textAreaProps.onKeyUp}
                  >
                    {typeof item === "object" ? item.en : item}
                  </textarea>
                </>
              )}

              {item.includeIf !== undefined && (
                <>
                  <label for={(textAreaProps.id ?? inputProps.id) + "-includeif-" + index}>IncludeIf</label>
                  <input
                    id={(textAreaProps.id ?? inputProps.id) + "-includeif-" + index}
                    class={textAreaProps.class ?? inputProps.class}
                    value={item.includeIf}
                    ref={inputRefs[refIndex++] as RefObject<HTMLInputElement>}
                    onChange={inputProps.onChange ?? textAreaProps.onChange}
                    onKeyUp={inputProps.onKeyUp ?? textAreaProps.onChange}
                  />
                </>
              )}
            </>
          ))
        ) : null}
      </div>
    );
  }
}
