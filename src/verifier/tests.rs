use super::*;
use crate::common::ownership::LifetimeId;
use crate::dtal::constraints::{Constraint, IndexExpr};
use crate::dtal::instr::{BinaryOp, CmpOp, DtalBlock, DtalInstr, TypeState};
use crate::dtal::regs::{PhysicalReg, Reg, VirtualReg};
use crate::dtal::types::DtalType;
use std::sync::Arc;

fn v(n: u32) -> Reg {
    Reg::Virtual(VirtualReg(n))
}
fn r0() -> Reg {
    Reg::Physical(PhysicalReg::R0)
}

fn make_program(functions: Vec<DtalFunction>) -> DtalProgram {
    DtalProgram { functions }
}

fn make_func(
    name: &str,
    params: Vec<(Reg, DtalType)>,
    return_type: DtalType,
    blocks: Vec<DtalBlock>,
) -> DtalFunction {
    let parameter_kinds = vec![crate::common::ownership::ParameterKind::PlainValue; params.len()];
    DtalFunction {
        name: name.to_string(),
        params,
        parameter_kinds,
        return_type,
        precondition: None,
        postcondition: None,
        blocks,
    }
}

fn make_block(label: &str, instructions: Vec<DtalInstr>) -> DtalBlock {
    DtalBlock {
        label: label.to_string(),
        entry_state: TypeState::new(),
        instructions,
    }
}

fn make_func_with_entry_state(
    name: &str,
    params: Vec<(Reg, DtalType)>,
    return_type: DtalType,
    entry_state: TypeState,
    instructions: Vec<DtalInstr>,
) -> DtalFunction {
    let parameter_kinds = vec![crate::common::ownership::ParameterKind::PlainValue; params.len()];
    DtalFunction {
        name: name.to_string(),
        params,
        parameter_kinds,
        return_type,
        precondition: None,
        postcondition: None,
        blocks: vec![DtalBlock {
            label: ".entry".to_string(),
            entry_state,
            instructions,
        }],
    }
}

#[test]
fn move_owned_and_drop_owned_update_owned_register_state() {
    let mut state = TypeState::new();
    let array_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(4),
    };
    state.register_types.insert(v(0), array_ty.clone());
    state.owned_registers.insert(v(0));
    state.owned_object_ids.insert(v(0), 0);

    let program = make_program(vec![]);

    verify_instruction(
        &DtalInstr::MoveOwned {
            dst: v(1),
            src: v(0),
            ty: array_ty.clone(),
        },
        &mut state,
        ".entry",
        &program,
    )
    .unwrap();

    assert!(!state.owned_registers.contains(&v(0)));
    assert!(state.owned_registers.contains(&v(1)));

    verify_instruction(
        &DtalInstr::DropOwned {
            src: v(1),
            ty: array_ty,
        },
        &mut state,
        ".entry",
        &program,
    )
    .unwrap();

    assert!(!state.owned_registers.contains(&v(1)));
}

#[test]
fn dtal_text_roundtrip_preserves_owned_annotations() {
    use crate::backend::emit::emit_program;

    let array_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(4),
    };
    let mut entry_state = TypeState::new();
    entry_state.register_types.insert(v(0), array_ty.clone());
    entry_state.owned_registers.insert(v(0));
    entry_state.owned_object_ids.insert(v(0), 0);

    let program = make_program(vec![DtalFunction {
        name: "owned_ops".to_string(),
        params: vec![],
        parameter_kinds: vec![],
        return_type: DtalType::Unit,
        precondition: None,
        postcondition: None,
        blocks: vec![DtalBlock {
            label: ".entry".to_string(),
            entry_state,
            instructions: vec![
                DtalInstr::MoveOwned {
                    dst: v(1),
                    src: v(0),
                    ty: array_ty.clone(),
                },
                DtalInstr::Call {
                    target: "owned_callee".to_string(),
                    arg_kinds: vec![],
                    return_ty: array_ty.clone(),
                    ownership: crate::common::ownership::OwnershipMode::FreshOwned,
                },
                DtalInstr::DropOwned {
                    src: v(1),
                    ty: array_ty,
                },
                DtalInstr::Ret,
            ],
        }],
    }]);

    let text = emit_program(&program);
    let parsed = crate::dtal::parser::parse_dtal(&text).unwrap();
    let block = &parsed.functions[0].blocks[0];

    assert!(block.entry_state.owned_registers.contains(&v(0)));
    assert!(matches!(
        &block.instructions[0],
        DtalInstr::MoveOwned { dst, src, .. } if *dst == v(1) && *src == v(0)
    ));
    assert!(matches!(
        &block.instructions[1],
        DtalInstr::Call {
            arg_kinds,
            ownership: crate::common::ownership::OwnershipMode::FreshOwned,
            ..
        } if arg_kinds.is_empty()
    ));
    assert!(matches!(
        &block.instructions[2],
        DtalInstr::DropOwned { src, .. } if *src == v(1)
    ));
}

#[test]
fn owned_call_marks_return_registers_owned() {
    let array_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(4),
    };
    let callee = make_func(
        "owned_callee",
        vec![],
        array_ty.clone(),
        vec![make_block(".entry", vec![DtalInstr::Ret])],
    );
    let program = make_program(vec![callee]);
    let mut state = TypeState::new();

    verify_instruction(
        &DtalInstr::Call {
            target: "owned_callee".to_string(),
            arg_kinds: vec![],
            return_ty: array_ty,
            ownership: crate::common::ownership::OwnershipMode::FreshOwned,
        },
        &mut state,
        ".entry",
        &program,
    )
    .unwrap();

    assert!(state.owned_registers.contains(&r0()));
    assert!(
        state
            .owned_registers
            .contains(&Reg::Physical(PhysicalReg::LR))
    );
}

#[test]
fn consuming_call_argument_is_consumed_at_call_boundary() {
    let array_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(4),
    };
    let callee = make_func(
        "consume_arr",
        vec![(v(0), DtalType::Int), (v(1), array_ty.clone())],
        DtalType::Unit,
        vec![make_block(".entry", vec![DtalInstr::Ret])],
    );
    let program = make_program(vec![callee]);
    let mut state = TypeState::new();
    let r1 = Reg::Physical(PhysicalReg::R1);
    state.register_types.insert(r1, array_ty);
    state.owned_registers.insert(r1);
    state.owned_object_ids.insert(r1, 7);

    verify_instruction(
        &DtalInstr::Call {
            target: "consume_arr".to_string(),
            arg_kinds: vec![
                crate::common::ownership::ParameterKind::PlainValue,
                crate::common::ownership::ParameterKind::OwnedValue,
            ],
            return_ty: DtalType::Unit,
            ownership: crate::common::ownership::OwnershipMode::Plain,
        },
        &mut state,
        ".entry",
        &program,
    )
    .unwrap();

    assert!(!state.owned_registers.contains(&r1));
    assert!(state.consumed_registers.contains(&r1));
}

#[test]
fn consumed_owned_register_cannot_be_used_again() {
    let array_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(4),
    };
    let mut state = TypeState::new();
    state.register_types.insert(v(0), array_ty.clone());
    state.owned_registers.insert(v(0));
    state.owned_object_ids.insert(v(0), 0);
    let program = make_program(vec![]);

    verify_instruction(
        &DtalInstr::MoveOwned {
            dst: v(1),
            src: v(0),
            ty: array_ty.clone(),
        },
        &mut state,
        ".entry",
        &program,
    )
    .unwrap();

    let err = verify_instruction(
        &DtalInstr::Load {
            dst: v(2),
            base: v(0),
            offset: v(3),
            ty: DtalType::Int,
        },
        &mut state,
        ".entry",
        &program,
    )
    .unwrap_err();

    assert!(matches!(err, VerifyError::ConsumedRegister { reg, .. } if reg == v(0)));
}

#[test]
fn double_drop_is_rejected() {
    let array_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(4),
    };
    let mut state = TypeState::new();
    state.register_types.insert(v(0), array_ty.clone());
    state.owned_registers.insert(v(0));
    state.owned_object_ids.insert(v(0), 0);
    let program = make_program(vec![]);

    verify_instruction(
        &DtalInstr::DropOwned {
            src: v(0),
            ty: array_ty.clone(),
        },
        &mut state,
        ".entry",
        &program,
    )
    .unwrap();

    let err = verify_instruction(
        &DtalInstr::DropOwned {
            src: v(0),
            ty: array_ty,
        },
        &mut state,
        ".entry",
        &program,
    )
    .unwrap_err();

    assert!(matches!(
        err,
        VerifyError::ConsumedRegister { reg, .. } if reg == v(0)
    ));
}

#[test]
fn plain_mov_of_owned_value_is_rejected_as_ownership_duplication() {
    let array_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(4),
    };
    let mut state = TypeState::new();
    state.register_types.insert(v(0), array_ty.clone());
    state.owned_registers.insert(v(0));
    state.owned_object_ids.insert(v(0), 7);
    let program = make_program(vec![]);

    let err = verify_instruction(
        &DtalInstr::MovReg {
            dst: v(1),
            src: v(0),
            ty: array_ty,
        },
        &mut state,
        ".entry",
        &program,
    )
    .unwrap_err();

    assert!(matches!(err, VerifyError::OwnershipViolation { .. }));
}

#[test]
fn duplicate_live_owners_of_same_object_are_rejected() {
    let array_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(4),
    };
    let mut state = TypeState::new();
    state.register_types.insert(v(0), array_ty.clone());
    state.register_types.insert(v(1), array_ty);
    state.owned_registers.insert(v(0));
    state.owned_registers.insert(v(1));
    state.owned_object_ids.insert(v(0), 11);
    state.owned_object_ids.insert(v(1), 11);

    let err = checker::verify_unique_owned_objects(&state, ".entry", "test").unwrap_err();
    assert!(matches!(err, VerifyError::OwnershipViolation { .. }));
}

#[test]
fn alias_borrow_allows_non_owning_alias_of_owned_value() {
    let array_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(4),
    };
    let mut state = TypeState::new();
    state.register_types.insert(v(0), array_ty.clone());
    state.owned_registers.insert(v(0));
    state.owned_object_ids.insert(v(0), 21);
    let program = make_program(vec![]);

    verify_instruction(
        &DtalInstr::AliasBorrow {
            lifetime: None,
            dst: v(1),
            src: v(0),
            ty: array_ty,
        },
        &mut state,
        ".entry",
        &program,
    )
    .unwrap();

    assert!(state.owned_registers.contains(&v(0)));
    assert!(!state.owned_registers.contains(&v(1)));
    assert_eq!(state.shared_borrow_object_ids.get(&v(1)).copied(), Some(21));
}

#[test]
fn move_owned_while_shared_borrow_live_is_rejected() {
    let array_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(4),
    };
    let mut state = TypeState::new();
    state.register_types.insert(v(0), array_ty.clone());
    state.register_types.insert(v(1), array_ty.clone());
    state.owned_registers.insert(v(0));
    state.owned_object_ids.insert(v(0), 31);
    state.shared_borrow_object_ids.insert(v(1), 31);
    let program = make_program(vec![]);

    let err = verify_instruction(
        &DtalInstr::MoveOwned {
            dst: v(2),
            src: v(0),
            ty: array_ty,
        },
        &mut state,
        ".entry",
        &program,
    )
    .unwrap_err();

    assert!(matches!(err, VerifyError::OwnershipViolation { .. }));
}

#[test]
fn drop_owned_while_shared_borrow_live_is_rejected() {
    let array_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(4),
    };
    let mut state = TypeState::new();
    state.register_types.insert(v(0), array_ty.clone());
    state.register_types.insert(v(1), array_ty.clone());
    state.owned_registers.insert(v(0));
    state.owned_object_ids.insert(v(0), 32);
    state.shared_borrow_object_ids.insert(v(1), 32);
    let program = make_program(vec![]);

    let err = verify_instruction(
        &DtalInstr::DropOwned {
            src: v(0),
            ty: array_ty,
        },
        &mut state,
        ".entry",
        &program,
    )
    .unwrap_err();

    assert!(matches!(err, VerifyError::OwnershipViolation { .. }));
}

#[test]
fn alias_borrow_outside_call_setup_is_allowed() {
    let array_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(4),
    };
    let program = make_program(vec![make_func(
        "main",
        vec![(v(0), array_ty.clone())],
        DtalType::Unit,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::AliasBorrow {
                    lifetime: None,
                    dst: v(1),
                    src: v(0),
                    ty: array_ty,
                },
                DtalInstr::Ret,
            ],
        )],
    )]);

    verify_dtal(&program).unwrap();
}

#[test]
fn alias_borrow_can_chain_from_existing_shared_borrow() {
    let array_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(4),
    };
    let mut state = TypeState::new();
    state.register_types.insert(v(0), array_ty.clone());
    state.owned_registers.insert(v(0));
    state.owned_object_ids.insert(v(0), 41);
    let program = make_program(vec![]);

    verify_instruction(
        &DtalInstr::AliasBorrow {
            lifetime: None,
            dst: v(1),
            src: v(0),
            ty: array_ty.clone(),
        },
        &mut state,
        ".entry",
        &program,
    )
    .unwrap();

    verify_instruction(
        &DtalInstr::AliasBorrow {
            lifetime: None,
            dst: v(2),
            src: v(1),
            ty: array_ty,
        },
        &mut state,
        ".entry",
        &program,
    )
    .unwrap();

    assert_eq!(state.shared_borrow_object_ids.get(&v(1)).copied(), Some(41));
    assert_eq!(state.shared_borrow_object_ids.get(&v(2)).copied(), Some(41));
    assert!(state.owned_registers.contains(&v(0)));
}

#[test]
fn borrow_end_releases_shared_borrow_so_owner_can_move() {
    let array_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(4),
    };
    let mut state = TypeState::new();
    state.register_types.insert(v(0), array_ty.clone());
    state.owned_registers.insert(v(0));
    state.owned_object_ids.insert(v(0), 51);
    let program = make_program(vec![]);

    verify_instruction(
        &DtalInstr::AliasBorrow {
            lifetime: None,
            dst: v(1),
            src: v(0),
            ty: array_ty.clone(),
        },
        &mut state,
        ".entry",
        &program,
    )
    .unwrap();

    verify_instruction(
        &DtalInstr::BorrowEnd {
            lifetime: None,
            src: v(1),
            ty: array_ty.clone(),
        },
        &mut state,
        ".entry",
        &program,
    )
    .unwrap();

    assert!(!state.shared_borrow_object_ids.contains_key(&v(1)));

    verify_instruction(
        &DtalInstr::MoveOwned {
            dst: v(2),
            src: v(0),
            ty: array_ty,
        },
        &mut state,
        ".entry",
        &program,
    )
    .unwrap();
}

#[test]
fn borrow_end_without_live_shared_borrow_is_rejected() {
    let mut state = TypeState::new();
    state.register_types.insert(v(0), DtalType::Int);
    let program = make_program(vec![]);

    let err = verify_instruction(
        &DtalInstr::BorrowEnd {
            lifetime: None,
            src: v(0),
            ty: DtalType::Int,
        },
        &mut state,
        ".entry",
        &program,
    )
    .unwrap_err();

    assert!(matches!(err, VerifyError::OwnershipViolation { .. }));
}

#[test]
fn borrow_end_with_wrong_lifetime_is_rejected() {
    let array_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(4),
    };
    let mut state = TypeState::new();
    state.register_types.insert(v(0), array_ty.clone());
    state.owned_registers.insert(v(0));
    state.owned_object_ids.insert(v(0), 52);
    let program = make_program(vec![]);

    verify_instruction(
        &DtalInstr::AliasBorrow {
            lifetime: Some(LifetimeId(1)),
            dst: v(1),
            src: v(0),
            ty: array_ty.clone(),
        },
        &mut state,
        ".entry",
        &program,
    )
    .unwrap();

    let err = verify_instruction(
        &DtalInstr::BorrowEnd {
            lifetime: Some(LifetimeId(2)),
            src: v(1),
            ty: array_ty,
        },
        &mut state,
        ".entry",
        &program,
    )
    .unwrap_err();

    assert!(matches!(err, VerifyError::OwnershipViolation { .. }));
}

#[test]
fn borrow_mut_of_owned_value_is_allowed() {
    let array_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(4),
    };
    let mut state = TypeState::new();
    state.register_types.insert(v(0), array_ty.clone());
    state.owned_registers.insert(v(0));
    state.owned_object_ids.insert(v(0), 61);
    let program = make_program(vec![]);

    verify_instruction(
        &DtalInstr::BorrowMut {
            lifetime: None,
            dst: v(1),
            src: v(0),
            ty: array_ty,
        },
        &mut state,
        ".entry",
        &program,
    )
    .unwrap();

    assert_eq!(
        state.mutable_borrow_object_ids.get(&v(1)).copied(),
        Some(61)
    );
    assert!(state.owned_registers.contains(&v(0)));
}

#[test]
fn borrow_mut_while_shared_borrow_live_is_rejected() {
    let array_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(4),
    };
    let mut state = TypeState::new();
    state.register_types.insert(v(0), array_ty.clone());
    state.register_types.insert(v(1), array_ty.clone());
    state.owned_registers.insert(v(0));
    state.owned_object_ids.insert(v(0), 62);
    state.shared_borrow_object_ids.insert(v(1), 62);
    let program = make_program(vec![]);

    let err = verify_instruction(
        &DtalInstr::BorrowMut {
            lifetime: None,
            dst: v(2),
            src: v(0),
            ty: array_ty,
        },
        &mut state,
        ".entry",
        &program,
    )
    .unwrap_err();

    assert!(matches!(err, VerifyError::OwnershipViolation { .. }));
}

#[test]
fn alias_borrow_while_mutable_borrow_live_is_rejected() {
    let array_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(4),
    };
    let mut state = TypeState::new();
    state.register_types.insert(v(0), array_ty.clone());
    state.register_types.insert(v(1), array_ty.clone());
    state.owned_registers.insert(v(0));
    state.owned_object_ids.insert(v(0), 63);
    state.mutable_borrow_object_ids.insert(v(1), 63);
    let program = make_program(vec![]);

    let err = verify_instruction(
        &DtalInstr::AliasBorrow {
            lifetime: None,
            dst: v(2),
            src: v(0),
            ty: array_ty,
        },
        &mut state,
        ".entry",
        &program,
    )
    .unwrap_err();

    assert!(matches!(err, VerifyError::OwnershipViolation { .. }));
}

#[test]
fn plain_mov_of_mutable_borrow_is_rejected() {
    let array_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(4),
    };
    let mut state = TypeState::new();
    state.register_types.insert(v(0), array_ty.clone());
    state.mutable_borrow_object_ids.insert(v(0), 64);
    let program = make_program(vec![]);

    let err = verify_instruction(
        &DtalInstr::MovReg {
            dst: v(1),
            src: v(0),
            ty: array_ty,
        },
        &mut state,
        ".entry",
        &program,
    )
    .unwrap_err();

    assert!(matches!(err, VerifyError::OwnershipViolation { .. }));
}

#[test]
fn branch_local_shared_borrow_is_rejected_at_join() {
    let array_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(4),
    };
    let program = make_program(vec![DtalFunction {
        name: "main".to_string(),
        params: vec![(v(0), array_ty.clone())],
        parameter_kinds: vec![crate::common::ownership::ParameterKind::OwnedValue],
        return_type: DtalType::Unit,
        precondition: None,
        postcondition: None,
        blocks: vec![
            make_block(
                ".entry",
                vec![
                    DtalInstr::MovImm {
                        dst: v(9),
                        imm: 0,
                        ty: DtalType::Int,
                    },
                    DtalInstr::Cmp {
                        lhs: v(9),
                        rhs: v(9),
                    },
                    DtalInstr::Branch {
                        cond: crate::dtal::instr::CmpOp::Eq,
                        target: ".then".to_string(),
                    },
                    DtalInstr::Jmp {
                        target: ".else".to_string(),
                    },
                ],
            ),
            make_block(
                ".then",
                vec![
                    DtalInstr::AliasBorrow {
                        lifetime: None,
                        dst: v(1),
                        src: v(0),
                        ty: array_ty.clone(),
                    },
                    DtalInstr::Jmp {
                        target: ".join".to_string(),
                    },
                ],
            ),
            make_block(
                ".else",
                vec![DtalInstr::Jmp {
                    target: ".join".to_string(),
                }],
            ),
            make_block(".join", vec![DtalInstr::Ret]),
        ],
    }]);

    let err = verify_dtal(&program).unwrap_err();
    assert!(matches!(err, VerifyError::OwnershipViolation { .. }));
}

#[test]
fn mixed_shared_and_mutable_borrow_is_rejected_at_join() {
    let array_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(4),
    };
    let program = make_program(vec![DtalFunction {
        name: "main".to_string(),
        params: vec![(v(0), array_ty.clone())],
        parameter_kinds: vec![crate::common::ownership::ParameterKind::OwnedValue],
        return_type: DtalType::Unit,
        precondition: None,
        postcondition: None,
        blocks: vec![
            make_block(
                ".entry",
                vec![
                    DtalInstr::MovImm {
                        dst: v(9),
                        imm: 0,
                        ty: DtalType::Int,
                    },
                    DtalInstr::Cmp {
                        lhs: v(9),
                        rhs: v(9),
                    },
                    DtalInstr::Branch {
                        cond: crate::dtal::instr::CmpOp::Eq,
                        target: ".then".to_string(),
                    },
                    DtalInstr::Jmp {
                        target: ".else".to_string(),
                    },
                ],
            ),
            make_block(
                ".then",
                vec![
                    DtalInstr::AliasBorrow {
                        lifetime: None,
                        dst: v(1),
                        src: v(0),
                        ty: array_ty.clone(),
                    },
                    DtalInstr::Jmp {
                        target: ".join".to_string(),
                    },
                ],
            ),
            make_block(
                ".else",
                vec![
                    DtalInstr::BorrowMut {
                        lifetime: None,
                        dst: v(2),
                        src: v(0),
                        ty: array_ty.clone(),
                    },
                    DtalInstr::Jmp {
                        target: ".join".to_string(),
                    },
                ],
            ),
            make_block(".join", vec![DtalInstr::Ret]),
        ],
    }]);

    let err = verify_dtal(&program).unwrap_err();
    assert!(matches!(err, VerifyError::OwnershipViolation { .. }));
}

#[test]
fn borrow_end_on_one_path_only_is_rejected_at_join() {
    let array_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(4),
    };
    let program = make_program(vec![DtalFunction {
        name: "main".to_string(),
        params: vec![(v(0), array_ty.clone())],
        parameter_kinds: vec![crate::common::ownership::ParameterKind::OwnedValue],
        return_type: DtalType::Unit,
        precondition: None,
        postcondition: None,
        blocks: vec![
            make_block(
                ".entry",
                vec![
                    DtalInstr::AliasBorrow {
                        lifetime: None,
                        dst: v(1),
                        src: v(0),
                        ty: array_ty.clone(),
                    },
                    DtalInstr::MovImm {
                        dst: v(9),
                        imm: 0,
                        ty: DtalType::Int,
                    },
                    DtalInstr::Cmp {
                        lhs: v(9),
                        rhs: v(9),
                    },
                    DtalInstr::Branch {
                        cond: crate::dtal::instr::CmpOp::Eq,
                        target: ".then".to_string(),
                    },
                    DtalInstr::Jmp {
                        target: ".else".to_string(),
                    },
                ],
            ),
            make_block(
                ".then",
                vec![
                    DtalInstr::BorrowEnd {
                        lifetime: None,
                        src: v(1),
                        ty: array_ty.clone(),
                    },
                    DtalInstr::Jmp {
                        target: ".join".to_string(),
                    },
                ],
            ),
            make_block(
                ".else",
                vec![DtalInstr::Jmp {
                    target: ".join".to_string(),
                }],
            ),
            make_block(".join", vec![DtalInstr::Ret]),
        ],
    }]);

    let err = verify_dtal(&program).unwrap_err();
    assert!(matches!(err, VerifyError::OwnershipViolation { .. }));
}

#[test]
fn loop_carried_shared_borrow_is_preserved() {
    let array_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(4),
    };
    let program = make_program(vec![DtalFunction {
        name: "main".to_string(),
        params: vec![(v(0), array_ty.clone())],
        parameter_kinds: vec![crate::common::ownership::ParameterKind::OwnedValue],
        return_type: DtalType::Unit,
        precondition: None,
        postcondition: None,
        blocks: vec![
            make_block(
                ".entry",
                vec![
                    DtalInstr::AliasBorrow {
                        lifetime: None,
                        dst: v(1),
                        src: v(0),
                        ty: array_ty.clone(),
                    },
                    DtalInstr::MovImm {
                        dst: v(9),
                        imm: 0,
                        ty: DtalType::Int,
                    },
                    DtalInstr::Jmp {
                        target: ".header".to_string(),
                    },
                ],
            ),
            make_block(
                ".header",
                vec![
                    DtalInstr::Cmp {
                        lhs: v(9),
                        rhs: v(9),
                    },
                    DtalInstr::Branch {
                        cond: crate::dtal::instr::CmpOp::Eq,
                        target: ".exit".to_string(),
                    },
                    DtalInstr::Jmp {
                        target: ".body".to_string(),
                    },
                ],
            ),
            make_block(
                ".body",
                vec![DtalInstr::Jmp {
                    target: ".header".to_string(),
                }],
            ),
            make_block(
                ".exit",
                vec![
                    DtalInstr::BorrowEnd {
                        lifetime: None,
                        src: v(1),
                        ty: array_ty,
                    },
                    DtalInstr::Ret,
                ],
            ),
        ],
    }]);

    verify_dtal(&program).unwrap();
}

#[test]
fn alias_borrow_for_consuming_call_argument_is_rejected() {
    let program = make_program(vec![
        make_func(
            "main",
            vec![(v(0), DtalType::Int)],
            DtalType::Unit,
            vec![make_block(
                ".entry",
                vec![
                    DtalInstr::AliasBorrow {
                        lifetime: None,
                        dst: r0(),
                        src: v(0),
                        ty: DtalType::Int,
                    },
                    DtalInstr::Call {
                        target: "f".to_string(),
                        arg_kinds: vec![crate::common::ownership::ParameterKind::OwnedValue],
                        return_ty: DtalType::Unit,
                        ownership: crate::common::ownership::OwnershipMode::Plain,
                    },
                    DtalInstr::Ret,
                ],
            )],
        ),
        make_func(
            "f",
            vec![(r0(), DtalType::Int)],
            DtalType::Unit,
            vec![make_block(".entry", vec![DtalInstr::Ret])],
        ),
    ]);

    let err = verify_dtal(&program).unwrap_err();
    assert!(matches!(err, VerifyError::OwnershipViolation { .. }));
}

#[test]
fn test_verify_simple_function() {
    let program = make_program(vec![make_func(
        "id",
        vec![(v(0), DtalType::Int)],
        DtalType::Int,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::MovReg {
                    dst: r0(),
                    src: v(0),
                    ty: DtalType::Int,
                },
                DtalInstr::Ret,
            ],
        )],
    )]);
    let result = verify_dtal(&program);
    assert!(result.is_ok(), "Verification failed: {:?}", result.err());
}

#[test]
fn test_verify_singleton_match() {
    let program = make_program(vec![make_func(
        "const_five",
        vec![],
        DtalType::SingletonInt(IndexExpr::Const(5)),
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::MovImm {
                    dst: v(0),
                    imm: 5,
                    ty: DtalType::SingletonInt(IndexExpr::Const(5)),
                },
                DtalInstr::MovReg {
                    dst: r0(),
                    src: v(0),
                    ty: DtalType::SingletonInt(IndexExpr::Const(5)),
                },
                DtalInstr::Ret,
            ],
        )],
    )]);
    assert!(verify_dtal(&program).is_ok());
}

#[test]
fn test_movimm_rejects_wrong_singleton_annotation() {
    let program = make_program(vec![make_func(
        "bad",
        vec![],
        DtalType::SingletonInt(IndexExpr::Const(5)),
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::MovImm {
                    dst: r0(),
                    imm: 5,
                    ty: DtalType::SingletonInt(IndexExpr::Const(6)),
                },
                DtalInstr::Ret,
            ],
        )],
    )]);
    let result = verify_dtal(&program);
    assert!(
        result.is_err(),
        "Wrong singleton annotation should be rejected"
    );
    assert!(matches!(
        result.unwrap_err(),
        VerifyError::SingletonMismatch { .. }
    ));
}

#[test]
fn test_movimm_accepts_correct_annotation() {
    let program = make_program(vec![make_func(
        "ok",
        vec![],
        DtalType::SingletonInt(IndexExpr::Const(5)),
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::MovImm {
                    dst: r0(),
                    imm: 5,
                    ty: DtalType::SingletonInt(IndexExpr::Const(5)),
                },
                DtalInstr::Ret,
            ],
        )],
    )]);
    assert!(verify_dtal(&program).is_ok());
}

#[test]
fn test_reject_undefined_register() {
    let program = make_program(vec![make_func(
        "bad",
        vec![],
        DtalType::Int,
        vec![make_block(
            ".entry",
            vec![DtalInstr::MovReg {
                dst: v(1),
                src: v(0),
                ty: DtalType::Int,
            }],
        )],
    )]);
    let result = verify_dtal(&program);
    assert!(matches!(
        result.unwrap_err(),
        VerifyError::UndefinedRegister { .. }
    ));
}

#[test]
fn test_entry_state_constraint_feeds_context() {
    let mut entry_state = TypeState::new();
    entry_state.register_types.insert(v(0), DtalType::Int);
    entry_state.constraints.push(Constraint::Lt(
        IndexExpr::Var("v0".to_string()),
        IndexExpr::Const(10),
    ));
    let program = make_program(vec![make_func_with_entry_state(
        "ok",
        vec![],
        DtalType::Int,
        entry_state,
        vec![DtalInstr::ConstraintAssert {
            constraint: Constraint::Lt(IndexExpr::Var("v0".to_string()), IndexExpr::Const(10)),
        }],
    )]);
    assert!(
        verify_dtal(&program).is_ok(),
        "Entry state constraint should be available in verification context"
    );
}

#[test]
fn test_constraint_assert_provable_from_precondition() {
    let mut func = make_func(
        "ok",
        vec![(v(0), DtalType::Int)],
        DtalType::Int,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::ConstraintAssert {
                    constraint: Constraint::Ge(
                        IndexExpr::Var("v0".to_string()),
                        IndexExpr::Const(0),
                    ),
                },
                DtalInstr::MovReg {
                    dst: r0(),
                    src: v(0),
                    ty: DtalType::Int,
                },
                DtalInstr::Ret,
            ],
        )],
    );
    func.precondition = Some(Constraint::Ge(
        IndexExpr::Var("v0".to_string()),
        IndexExpr::Const(0),
    ));
    let program = make_program(vec![func]);
    assert!(
        verify_dtal(&program).is_ok(),
        "Precondition should make the assert provable"
    );
}

#[test]
fn test_constraint_assert_unprovable() {
    let program = make_program(vec![make_func(
        "bad",
        vec![(v(0), DtalType::Int)],
        DtalType::Int,
        vec![make_block(
            ".entry",
            vec![DtalInstr::ConstraintAssert {
                constraint: Constraint::Gt(IndexExpr::Var("v0".to_string()), IndexExpr::Const(0)),
            }],
        )],
    )]);
    let result = verify_dtal(&program);
    assert!(matches!(
        result.unwrap_err(),
        VerifyError::UnprovableConstraint { .. }
    ));
}

#[test]
fn test_constraint_assert_z3_transitivity() {
    let mut func = make_func(
        "ok",
        vec![(v(0), DtalType::Int)],
        DtalType::Int,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::ConstraintAssert {
                    constraint: Constraint::Ge(
                        IndexExpr::Var("v0".to_string()),
                        IndexExpr::Const(3),
                    ),
                },
                DtalInstr::MovReg {
                    dst: r0(),
                    src: v(0),
                    ty: DtalType::Int,
                },
                DtalInstr::Ret,
            ],
        )],
    );
    func.precondition = Some(Constraint::Ge(
        IndexExpr::Var("v0".to_string()),
        IndexExpr::Const(5),
    ));
    let program = make_program(vec![func]);
    assert!(
        verify_dtal(&program).is_ok(),
        "x >= 5 should imply x >= 3 via Z3"
    );
}

#[test]
fn test_movreg_derives_from_source_ignoring_annotation() {
    let program = make_program(vec![make_func(
        "ok",
        vec![(v(0), DtalType::Int)],
        DtalType::Int,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::MovReg {
                    dst: r0(),
                    src: v(0),
                    ty: DtalType::SingletonInt(IndexExpr::Const(5)),
                },
                DtalInstr::Ret,
            ],
        )],
    )]);
    assert!(verify_dtal(&program).is_ok());
}

#[test]
fn test_singleton_subtype_of_int() {
    let program = make_program(vec![make_func(
        "ok",
        vec![(v(0), DtalType::SingletonInt(IndexExpr::Const(5)))],
        DtalType::Int,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::MovReg {
                    dst: r0(),
                    src: v(0),
                    ty: DtalType::Int,
                },
                DtalInstr::Ret,
            ],
        )],
    )]);
    assert!(verify_dtal(&program).is_ok());
}

#[test]
fn test_reject_bool_as_int_at_return() {
    let program = make_program(vec![make_func(
        "bad",
        vec![(v(0), DtalType::Int), (v(1), DtalType::Int)],
        DtalType::Int,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::Cmp {
                    lhs: v(0),
                    rhs: v(1),
                },
                DtalInstr::SetCC {
                    dst: v(2),
                    cond: CmpOp::Lt,
                },
                DtalInstr::MovReg {
                    dst: r0(),
                    src: v(2),
                    ty: DtalType::Int,
                },
                DtalInstr::Ret,
            ],
        )],
    )]);
    let result = verify_dtal(&program);
    assert!(result.is_err(), "Bool is not a subtype of Int at return");
    assert!(matches!(
        result.unwrap_err(),
        VerifyError::ReturnTypeMismatch { .. }
    ));
}

#[test]
fn test_array_subtyping_compatible() {
    let arr_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(10),
    };
    assert!(checker::types_compatible(&arr_ty, &arr_ty));
}

#[test]
fn test_array_subtyping_different_sizes() {
    let arr10 = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(10),
    };
    let arr5 = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(5),
    };
    assert!(!checker::types_compatible(&arr10, &arr5));
}

#[test]
fn test_reject_wrong_type_annotation() {
    let program = make_program(vec![make_func(
        "bad",
        vec![],
        DtalType::Int,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::MovImm {
                    dst: v(0),
                    imm: 5,
                    ty: DtalType::SingletonInt(IndexExpr::Const(5)),
                },
                DtalInstr::TypeAnnotation {
                    reg: v(0),
                    ty: DtalType::Bool,
                },
            ],
        )],
    )]);
    let result = verify_dtal(&program);
    assert!(
        result.is_err(),
        "TypeAnnotation should be rejected when incompatible"
    );
    assert!(matches!(
        result.unwrap_err(),
        VerifyError::TypeMismatch { .. }
    ));
}

#[test]
fn test_accept_valid_type_annotation() {
    let program = make_program(vec![make_func(
        "ok",
        vec![],
        DtalType::Int,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::MovImm {
                    dst: v(0),
                    imm: 5,
                    ty: DtalType::SingletonInt(IndexExpr::Const(5)),
                },
                DtalInstr::TypeAnnotation {
                    reg: v(0),
                    ty: DtalType::Int,
                },
                DtalInstr::MovReg {
                    dst: r0(),
                    src: v(0),
                    ty: DtalType::Int,
                },
                DtalInstr::Ret,
            ],
        )],
    )]);
    assert!(verify_dtal(&program).is_ok());
}

#[test]
fn test_accept_phi_type_annotation() {
    let program = make_program(vec![make_func(
        "ok",
        vec![],
        DtalType::Int,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::TypeAnnotation {
                    reg: v(0),
                    ty: DtalType::Int,
                },
                DtalInstr::MovReg {
                    dst: r0(),
                    src: v(0),
                    ty: DtalType::Int,
                },
                DtalInstr::Ret,
            ],
        )],
    )]);
    assert!(verify_dtal(&program).is_ok());
}

#[test]
fn test_reject_type_annotation_int_to_singleton() {
    let program = make_program(vec![make_func(
        "bad",
        vec![(v(0), DtalType::Int)],
        DtalType::Int,
        vec![make_block(
            ".entry",
            vec![DtalInstr::TypeAnnotation {
                reg: v(0),
                ty: DtalType::SingletonInt(IndexExpr::Const(5)),
            }],
        )],
    )]);
    let result = verify_dtal(&program);
    assert!(
        result.is_err(),
        "Cannot narrow Int to SingletonInt via annotation"
    );
}

#[test]
fn test_reject_load_without_bounds_proof() {
    let arr_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(10),
    };
    let program = make_program(vec![make_func(
        "bad",
        vec![(v(0), arr_ty), (v(1), DtalType::Int)],
        DtalType::Int,
        vec![make_block(
            ".entry",
            vec![DtalInstr::Load {
                dst: v(2),
                base: v(0),
                offset: v(1),
                ty: DtalType::Int,
            }],
        )],
    )]);
    let result = verify_dtal(&program);
    assert!(
        result.is_err(),
        "Load without bounds proof should be rejected"
    );
    assert!(matches!(
        result.unwrap_err(),
        VerifyError::BoundsCheckFailed { .. }
    ));
}

#[test]
fn test_accept_load_with_constant_in_bounds() {
    let arr_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(10),
    };
    let program = make_program(vec![make_func(
        "ok",
        vec![(v(0), arr_ty)],
        DtalType::Int,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::MovImm {
                    dst: v(1),
                    imm: 3,
                    ty: DtalType::SingletonInt(IndexExpr::Const(3)),
                },
                DtalInstr::ConstraintAssert {
                    constraint: Constraint::True,
                },
                DtalInstr::Load {
                    dst: v(2),
                    base: v(0),
                    offset: v(1),
                    ty: DtalType::Int,
                },
                DtalInstr::Ret,
            ],
        )],
    )]);
    let _result = verify_dtal(&program);
}

#[test]
fn test_accept_load_with_precondition_bounds() {
    let arr_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(10),
    };
    let mut func = make_func(
        "ok",
        vec![(v(0), arr_ty), (v(1), DtalType::Int)],
        DtalType::Int,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::Load {
                    dst: v(2),
                    base: v(0),
                    offset: v(1),
                    ty: DtalType::Int,
                },
                DtalInstr::MovReg {
                    dst: r0(),
                    src: v(2),
                    ty: DtalType::Int,
                },
                DtalInstr::Ret,
            ],
        )],
    );
    func.precondition = Some(Constraint::And(
        Box::new(Constraint::Ge(
            IndexExpr::Var("v1".to_string()),
            IndexExpr::Const(0),
        )),
        Box::new(Constraint::Lt(
            IndexExpr::Var("v1".to_string()),
            IndexExpr::Const(10),
        )),
    ));
    let program = make_program(vec![func]);
    assert!(
        verify_dtal(&program).is_ok(),
        "Precondition provides bounds proof for load"
    );
}

#[test]
fn test_reject_loadop_without_bounds_proof() {
    let arr_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(10),
    };
    let program = make_program(vec![make_func(
        "bad",
        vec![(v(0), arr_ty), (v(1), DtalType::Int), (v(2), DtalType::Int)],
        DtalType::Int,
        vec![make_block(
            ".entry",
            vec![DtalInstr::LoadOp {
                op: BinaryOp::Add,
                dst: v(3),
                base: v(0),
                offset: v(1),
                other: v(2),
                ty: DtalType::Int,
            }],
        )],
    )]);
    let result = verify_dtal(&program);
    assert!(
        matches!(result, Err(VerifyError::BoundsCheckFailed { .. })),
        "LoadOp without bounds proof should be rejected"
    );
}

#[test]
fn test_accept_loadop_with_precondition_bounds() {
    let arr_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(10),
    };
    let mut func = make_func(
        "ok",
        vec![(v(0), arr_ty), (v(1), DtalType::Int), (v(2), DtalType::Int)],
        DtalType::Int,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::LoadOp {
                    op: BinaryOp::Add,
                    dst: v(3),
                    base: v(0),
                    offset: v(1),
                    other: v(2),
                    ty: DtalType::Int,
                },
                DtalInstr::MovReg {
                    dst: r0(),
                    src: v(3),
                    ty: DtalType::Int,
                },
                DtalInstr::Ret,
            ],
        )],
    );
    func.precondition = Some(Constraint::And(
        Box::new(Constraint::Ge(
            IndexExpr::Var("v1".to_string()),
            IndexExpr::Const(0),
        )),
        Box::new(Constraint::Lt(
            IndexExpr::Var("v1".to_string()),
            IndexExpr::Const(10),
        )),
    ));
    let program = make_program(vec![func]);
    assert!(
        verify_dtal(&program).is_ok(),
        "Precondition provides bounds proof for loadop"
    );
}

#[test]
fn test_reject_store_without_bounds_proof() {
    let arr_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(5),
    };
    let program = make_program(vec![make_func(
        "bad",
        vec![(v(0), arr_ty), (v(1), DtalType::Int), (v(2), DtalType::Int)],
        DtalType::Int,
        vec![make_block(
            ".entry",
            vec![DtalInstr::Store {
                base: v(0),
                offset: v(1),
                src: v(2),
            }],
        )],
    )]);
    let result = verify_dtal(&program);
    assert!(
        result.is_err(),
        "Store without bounds proof should be rejected"
    );
    assert!(matches!(
        result.unwrap_err(),
        VerifyError::BoundsCheckFailed { .. }
    ));
}

#[test]
fn test_reject_unprovable_postcondition() {
    let mut func = make_func(
        "bad",
        vec![(v(0), DtalType::Int)],
        DtalType::Int,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::MovReg {
                    dst: r0(),
                    src: v(0),
                    ty: DtalType::Int,
                },
                DtalInstr::Ret,
            ],
        )],
    );
    func.postcondition = Some(Constraint::Gt(
        IndexExpr::Var("r0".to_string()),
        IndexExpr::Const(0),
    ));
    let program = make_program(vec![func]);
    let result = verify_dtal(&program);
    assert!(
        result.is_err(),
        "Postcondition should fail without supporting context"
    );
    assert!(matches!(
        result.unwrap_err(),
        VerifyError::PostconditionFailed { .. }
    ));
}

#[test]
fn test_accept_provable_postcondition() {
    let mut func = make_func(
        "ok",
        vec![(v(0), DtalType::Int)],
        DtalType::Int,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::MovReg {
                    dst: r0(),
                    src: v(0),
                    ty: DtalType::Int,
                },
                DtalInstr::Ret,
            ],
        )],
    );
    func.precondition = Some(Constraint::Gt(
        IndexExpr::Var("v0".to_string()),
        IndexExpr::Const(0),
    ));
    func.postcondition = Some(Constraint::Gt(
        IndexExpr::Var("v0".to_string()),
        IndexExpr::Const(0),
    ));
    let program = make_program(vec![func]);
    assert!(
        verify_dtal(&program).is_ok(),
        "Postcondition should be provable from precondition"
    );
}

#[test]
fn test_reject_unprovable_precondition_at_call() {
    let callee = {
        let mut f = make_func(
            "requires_positive",
            vec![(v(0), DtalType::Int)],
            DtalType::Int,
            vec![make_block(
                ".entry",
                vec![
                    DtalInstr::MovReg {
                        dst: r0(),
                        src: v(0),
                        ty: DtalType::Int,
                    },
                    DtalInstr::Ret,
                ],
            )],
        );
        f.precondition = Some(Constraint::Gt(
            IndexExpr::Var("v0".to_string()),
            IndexExpr::Const(0),
        ));
        f
    };
    let caller = make_func(
        "caller",
        vec![(v(0), DtalType::Int)],
        DtalType::Int,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::Call {
                    target: "requires_positive".to_string(),
                    arg_kinds: vec![],
                    return_ty: DtalType::Int,
                    ownership: crate::common::ownership::OwnershipMode::Plain,
                },
                DtalInstr::Ret,
            ],
        )],
    );
    let program = make_program(vec![callee, caller]);
    let result = verify_dtal(&program);
    assert!(
        result.is_err(),
        "Caller should fail to prove callee's precondition"
    );
    assert!(matches!(
        result.unwrap_err(),
        VerifyError::PreconditionFailed { .. }
    ));
}

#[test]
fn test_branch_derives_constraint_for_assert() {
    let mut func = make_func(
        "ok",
        vec![(v(0), DtalType::Int)],
        DtalType::Int,
        vec![
            make_block(
                ".entry",
                vec![
                    DtalInstr::CmpImm { lhs: v(0), imm: 10 },
                    DtalInstr::Branch {
                        cond: CmpOp::Lt,
                        target: ".taken".to_string(),
                    },
                    DtalInstr::ConstraintAssert {
                        constraint: Constraint::Ge(
                            IndexExpr::Var("v0".to_string()),
                            IndexExpr::Const(10),
                        ),
                    },
                    DtalInstr::MovReg {
                        dst: r0(),
                        src: v(0),
                        ty: DtalType::Int,
                    },
                    DtalInstr::Ret,
                ],
            ),
            make_block(
                ".taken",
                vec![
                    DtalInstr::MovReg {
                        dst: r0(),
                        src: v(0),
                        ty: DtalType::Int,
                    },
                    DtalInstr::Ret,
                ],
            ),
        ],
    );
    func.params = vec![(v(0), DtalType::Int)];
    let program = make_program(vec![func]);
    assert!(
        verify_dtal(&program).is_ok(),
        "Fall-through constraint v0 >= 10 should be derived from blt"
    );
}

#[test]
fn test_fallthrough_propagates_state() {
    let program = make_program(vec![make_func(
        "ok",
        vec![(v(0), DtalType::Int)],
        DtalType::Int,
        vec![
            make_block(
                ".bb0",
                vec![
                    DtalInstr::MovImm {
                        dst: v(1),
                        imm: 42,
                        ty: DtalType::Int,
                    },
                    DtalInstr::CmpImm { lhs: v(0), imm: 0 },
                    DtalInstr::Branch {
                        cond: CmpOp::Eq,
                        target: ".bb2".to_string(),
                    },
                ],
            ),
            make_block(
                ".bb1",
                vec![
                    DtalInstr::MovReg {
                        dst: r0(),
                        src: v(1),
                        ty: DtalType::Int,
                    },
                    DtalInstr::Ret,
                ],
            ),
            make_block(
                ".bb2",
                vec![
                    DtalInstr::MovReg {
                        dst: r0(),
                        src: v(1),
                        ty: DtalType::Int,
                    },
                    DtalInstr::Ret,
                ],
            ),
        ],
    )]);
    assert!(
        verify_dtal(&program).is_ok(),
        "Fall-through should propagate v1 to .bb1"
    );
}

#[test]
fn test_reject_return_type_mismatch() {
    let program = make_program(vec![make_func(
        "bad",
        vec![(v(0), DtalType::Int)],
        DtalType::Bool,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::MovReg {
                    dst: r0(),
                    src: v(0),
                    ty: DtalType::Int,
                },
                DtalInstr::Ret,
            ],
        )],
    )]);
    let result = verify_dtal(&program);
    assert!(matches!(
        result.unwrap_err(),
        VerifyError::ReturnTypeMismatch { .. }
    ));
}

#[test]
fn test_binop_derives_symbolic_type() {
    let program = make_program(vec![make_func(
        "ok",
        vec![],
        DtalType::Int,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::MovImm {
                    dst: v(0),
                    imm: 3,
                    ty: DtalType::Int,
                },
                DtalInstr::MovImm {
                    dst: v(1),
                    imm: 5,
                    ty: DtalType::Int,
                },
                DtalInstr::BinOp {
                    op: crate::dtal::instr::BinaryOp::Add,
                    dst: v(2),
                    lhs: v(0),
                    rhs: v(1),
                    ty: DtalType::Int,
                },
                DtalInstr::MovReg {
                    dst: r0(),
                    src: v(2),
                    ty: DtalType::Int,
                },
                DtalInstr::Ret,
            ],
        )],
    )]);
    assert!(verify_dtal(&program).is_ok());
}

#[test]
fn test_addi_derives_symbolic_type() {
    let program = make_program(vec![make_func(
        "ok",
        vec![],
        DtalType::Int,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::MovImm {
                    dst: v(0),
                    imm: 5,
                    ty: DtalType::Int,
                },
                DtalInstr::AddImm {
                    dst: v(1),
                    src: v(0),
                    imm: 10,
                    ty: DtalType::Int,
                },
                DtalInstr::MovReg {
                    dst: r0(),
                    src: v(1),
                    ty: DtalType::Int,
                },
                DtalInstr::Ret,
            ],
        )],
    )]);
    assert!(verify_dtal(&program).is_ok());
}

#[test]
fn test_singleton_coercion_via_constraint() {
    let constraints = vec![Constraint::Eq(
        IndexExpr::Var("n".to_string()),
        IndexExpr::Const(5),
    )];
    let actual = DtalType::SingletonInt(IndexExpr::Var("n".to_string()));
    let expected = DtalType::SingletonInt(IndexExpr::Const(5));
    assert!(checker::types_compatible_with_constraints(
        &actual,
        &expected,
        &constraints
    ));
}

#[test]
fn test_array_size_coercion_via_constraint() {
    let constraints = vec![Constraint::Eq(
        IndexExpr::Var("n".to_string()),
        IndexExpr::Const(5),
    )];
    let actual = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Var("n".to_string()),
    };
    let expected = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(5),
    };
    assert!(checker::types_compatible_with_constraints(
        &actual,
        &expected,
        &constraints
    ));
}

#[test]
fn test_declared_entry_state_verified() {
    use crate::dtal::instr::TypeState;

    let mut entry_state = TypeState::new();
    entry_state.register_types.insert(v(0), DtalType::Int);

    let program = make_program(vec![DtalFunction {
        name: "ok".to_string(),
        params: vec![(v(0), DtalType::Int)],
        parameter_kinds: vec![crate::common::ownership::ParameterKind::OwnedValue],
        return_type: DtalType::Int,
        precondition: None,
        postcondition: None,
        blocks: vec![DtalBlock {
            label: ".entry".to_string(),
            entry_state,
            instructions: vec![
                DtalInstr::MovReg {
                    dst: r0(),
                    src: v(0),
                    ty: DtalType::Int,
                },
                DtalInstr::Ret,
            ],
        }],
    }]);
    assert!(verify_dtal(&program).is_ok());
}

#[test]
fn test_state_coercion_at_jump() {
    use crate::dtal::instr::TypeState;

    let mut entry_state = TypeState::new();
    entry_state
        .register_types
        .insert(v(0), DtalType::SingletonInt(IndexExpr::Const(5)));

    let mut target_state = TypeState::new();
    target_state.register_types.insert(v(0), DtalType::Int);

    let program = make_program(vec![DtalFunction {
        name: "ok".to_string(),
        params: vec![],
        parameter_kinds: vec![],
        return_type: DtalType::Int,
        precondition: None,
        postcondition: None,
        blocks: vec![
            DtalBlock {
                label: ".entry".to_string(),
                entry_state,
                instructions: vec![DtalInstr::Jmp {
                    target: ".target".to_string(),
                }],
            },
            DtalBlock {
                label: ".target".to_string(),
                entry_state: target_state,
                instructions: vec![
                    DtalInstr::MovReg {
                        dst: r0(),
                        src: v(0),
                        ty: DtalType::Int,
                    },
                    DtalInstr::Ret,
                ],
            },
        ],
    }]);
    assert!(verify_dtal(&program).is_ok());
}

#[test]
fn test_reject_wrong_entry_state() {
    use crate::dtal::instr::TypeState;

    let mut entry_state = TypeState::new();
    entry_state
        .register_types
        .insert(v(0), DtalType::SingletonInt(IndexExpr::Const(5)));

    let mut target_state = TypeState::new();
    target_state.register_types.insert(v(0), DtalType::Bool);

    let program = make_program(vec![DtalFunction {
        name: "bad".to_string(),
        params: vec![],
        parameter_kinds: vec![],
        return_type: DtalType::Int,
        precondition: None,
        postcondition: None,
        blocks: vec![
            DtalBlock {
                label: ".entry".to_string(),
                entry_state,
                instructions: vec![DtalInstr::Jmp {
                    target: ".target".to_string(),
                }],
            },
            DtalBlock {
                label: ".target".to_string(),
                entry_state: target_state,
                instructions: vec![DtalInstr::Ret],
            },
        ],
    }]);
    let result = verify_dtal(&program);
    assert!(result.is_err(), "State coercion should fail: int(5) ≤ Bool");
}

#[test]
fn test_not_derives_bool_ignoring_annotation() {
    let program = make_program(vec![make_func(
        "ok",
        vec![(v(0), DtalType::Bool)],
        DtalType::Bool,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::Not {
                    dst: r0(),
                    src: v(0),
                    ty: DtalType::Int,
                },
                DtalInstr::Ret,
            ],
        )],
    )]);
    assert!(verify_dtal(&program).is_ok());
}

#[test]
fn test_not_derived_bool_fails_int_return() {
    let program = make_program(vec![make_func(
        "bad",
        vec![(v(0), DtalType::Bool)],
        DtalType::Int,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::Not {
                    dst: r0(),
                    src: v(0),
                    ty: DtalType::Int,
                },
                DtalInstr::Ret,
            ],
        )],
    )]);
    let result = verify_dtal(&program);
    assert!(result.is_err(), "Not derives Bool, not Int");
    assert!(matches!(
        result.unwrap_err(),
        VerifyError::ReturnTypeMismatch { .. }
    ));
}

#[test]
fn test_load_derives_element_type_from_array() {
    let arr_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Bool),
        size: IndexExpr::Const(10),
    };
    let mut func = make_func(
        "ok",
        vec![(v(0), arr_ty), (v(1), DtalType::Int)],
        DtalType::Bool,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::Load {
                    dst: r0(),
                    base: v(0),
                    offset: v(1),
                    ty: DtalType::Int,
                },
                DtalInstr::Ret,
            ],
        )],
    );
    func.precondition = Some(Constraint::And(
        Box::new(Constraint::Ge(
            IndexExpr::Var("v1".to_string()),
            IndexExpr::Const(0),
        )),
        Box::new(Constraint::Lt(
            IndexExpr::Var("v1".to_string()),
            IndexExpr::Const(10),
        )),
    ));
    let program = make_program(vec![func]);
    assert!(
        verify_dtal(&program).is_ok(),
        "Load should derive element type from array base"
    );
}

#[test]
fn test_pointer_arithmetic_over_nested_array_derives_row_type() {
    let row_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(4),
    };
    let matrix_ty = DtalType::Array {
        element_type: Arc::new(row_ty.clone()),
        size: IndexExpr::Const(4),
    };

    let mut func = make_func(
        "row_store",
        vec![
            (v(0), matrix_ty),
            (v(1), DtalType::Int),
            (v(2), DtalType::Int),
            (v(3), DtalType::Int),
        ],
        DtalType::Int,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::MovImm {
                    dst: v(4),
                    imm: 32,
                    ty: DtalType::SingletonInt(IndexExpr::Const(32)),
                },
                DtalInstr::BinOp {
                    op: crate::dtal::instr::BinaryOp::Mul,
                    dst: v(5),
                    lhs: v(1),
                    rhs: v(4),
                    ty: DtalType::Int,
                },
                DtalInstr::BinOp {
                    op: crate::dtal::instr::BinaryOp::Add,
                    dst: v(6),
                    lhs: v(0),
                    rhs: v(5),
                    ty: row_ty,
                },
                DtalInstr::Store {
                    base: v(6),
                    offset: v(2),
                    src: v(3),
                },
                DtalInstr::MovReg {
                    dst: r0(),
                    src: v(3),
                    ty: DtalType::Int,
                },
                DtalInstr::Ret,
            ],
        )],
    );
    func.precondition = Some(Constraint::And(
        Box::new(Constraint::Ge(
            IndexExpr::Var("v2".to_string()),
            IndexExpr::Const(0),
        )),
        Box::new(Constraint::Lt(
            IndexExpr::Var("v2".to_string()),
            IndexExpr::Const(4),
        )),
    ));

    let program = make_program(vec![func]);
    assert!(verify_dtal(&program).is_ok());
}

#[test]
fn test_call_derives_return_type_from_signature() {
    let callee = make_func(
        "returns_bool",
        vec![],
        DtalType::Bool,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::MovImm {
                    dst: r0(),
                    imm: 1,
                    ty: DtalType::Int,
                },
                DtalInstr::Ret,
            ],
        )],
    );
    let caller = make_func(
        "caller",
        vec![],
        DtalType::Bool,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::Call {
                    target: "returns_bool".to_string(),
                    arg_kinds: vec![],
                    return_ty: DtalType::Int,
                    ownership: crate::common::ownership::OwnershipMode::Plain,
                },
                DtalInstr::Ret,
            ],
        )],
    );
    let program = make_program(vec![callee, caller]);
    let result = verify_dtal(&program);
    if let Err(VerifyError::ReturnTypeMismatch { function, .. }) = result {
        assert_eq!(
            function, "returns_bool",
            "Error should be in callee, not caller"
        );
    }
}

#[test]
fn test_call_wrong_annotation_caught_via_signature() {
    let callee = make_func(
        "returns_bool",
        vec![],
        DtalType::Bool,
        vec![make_block(".entry", vec![DtalInstr::Ret])],
    );
    let caller = make_func(
        "caller",
        vec![],
        DtalType::Int,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::Call {
                    target: "returns_bool".to_string(),
                    arg_kinds: vec![],
                    return_ty: DtalType::Int,
                    ownership: crate::common::ownership::OwnershipMode::Plain,
                },
                DtalInstr::Ret,
            ],
        )],
    );
    let program = make_program(vec![callee, caller]);
    let result = verify_dtal(&program);
    assert!(result.is_err());
    if let VerifyError::ReturnTypeMismatch { function, .. } = result.unwrap_err() {
        assert_eq!(function, "caller");
    } else {
        panic!("Expected ReturnTypeMismatch from caller");
    }
}

#[test]
fn test_push_pop_derives_type_from_stack() {
    let program = make_program(vec![make_func(
        "ok",
        vec![],
        DtalType::SingletonInt(IndexExpr::Const(5)),
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::MovImm {
                    dst: v(0),
                    imm: 5,
                    ty: DtalType::Int,
                },
                DtalInstr::Push {
                    src: v(0),
                    ty: DtalType::Int,
                },
                DtalInstr::Pop {
                    dst: r0(),
                    ty: DtalType::Int,
                },
                DtalInstr::Ret,
            ],
        )],
    )]);
    assert!(verify_dtal(&program).is_ok());
}

#[test]
fn test_push_pop_bool_derives_correctly() {
    let program = make_program(vec![make_func(
        "ok",
        vec![(v(0), DtalType::Int), (v(1), DtalType::Int)],
        DtalType::Bool,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::Cmp {
                    lhs: v(0),
                    rhs: v(1),
                },
                DtalInstr::SetCC {
                    dst: v(2),
                    cond: CmpOp::Lt,
                },
                DtalInstr::Push {
                    src: v(2),
                    ty: DtalType::Int,
                },
                DtalInstr::Pop {
                    dst: r0(),
                    ty: DtalType::Int,
                },
                DtalInstr::Ret,
            ],
        )],
    )]);
    assert!(verify_dtal(&program).is_ok());
}

fn lr() -> Reg {
    Reg::Physical(PhysicalReg::LR)
}
fn r7() -> Reg {
    Reg::Physical(PhysicalReg::R7)
}

#[test]
fn test_physical_simple_return() {
    let program = make_program(vec![make_func(
        "main",
        vec![],
        DtalType::SingletonInt(IndexExpr::Const(42)),
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::Prologue {
                    frame_size: 0,
                    callee_saved: vec![],
                },
                DtalInstr::MovImm {
                    dst: lr(),
                    imm: 42,
                    ty: DtalType::SingletonInt(IndexExpr::Const(42)),
                },
                DtalInstr::Epilogue {
                    callee_saved: vec![],
                },
                DtalInstr::Ret,
            ],
        )],
    )]);
    assert!(verify_dtal(&program).is_ok());
}

#[test]
fn test_physical_return_type_mismatch() {
    let program = make_program(vec![make_func(
        "bad",
        vec![],
        DtalType::Bool,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::Prologue {
                    frame_size: 0,
                    callee_saved: vec![],
                },
                DtalInstr::MovImm {
                    dst: lr(),
                    imm: 42,
                    ty: DtalType::SingletonInt(IndexExpr::Const(42)),
                },
                DtalInstr::Epilogue {
                    callee_saved: vec![],
                },
                DtalInstr::Ret,
            ],
        )],
    )]);
    let result = verify_dtal(&program);
    assert!(result.is_err());
    assert!(matches!(
        result.unwrap_err(),
        VerifyError::ReturnTypeMismatch { .. }
    ));
}

#[test]
fn test_physical_r0_not_checked_for_return() {
    let program = make_program(vec![make_func(
        "main",
        vec![],
        DtalType::Int,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::Prologue {
                    frame_size: 0,
                    callee_saved: vec![],
                },
                DtalInstr::Alloca {
                    dst: r0(),
                    size: 24,
                    ty: DtalType::Array {
                        element_type: Arc::new(DtalType::Int),
                        size: IndexExpr::Const(3),
                    },
                },
                DtalInstr::MovImm {
                    dst: lr(),
                    imm: 7,
                    ty: DtalType::SingletonInt(IndexExpr::Const(7)),
                },
                DtalInstr::Epilogue {
                    callee_saved: vec![],
                },
                DtalInstr::Ret,
            ],
        )],
    )]);
    assert!(
        verify_dtal(&program).is_ok(),
        "Physical return should check LR, not R0"
    );
}

#[test]
fn test_physical_cqo_idiv() {
    let program = make_program(vec![make_func(
        "div",
        vec![(r0(), DtalType::Int)],
        DtalType::Int,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::Prologue {
                    frame_size: 0,
                    callee_saved: vec![],
                },
                DtalInstr::MovImm {
                    dst: r7(),
                    imm: 10,
                    ty: DtalType::SingletonInt(IndexExpr::Const(10)),
                },
                DtalInstr::MovReg {
                    dst: lr(),
                    src: r0(),
                    ty: DtalType::Int,
                },
                DtalInstr::Cqo,
                DtalInstr::Idiv { src: r7() },
                DtalInstr::Epilogue {
                    callee_saved: vec![],
                },
                DtalInstr::Ret,
            ],
        )],
    )]);
    assert!(
        verify_dtal(&program).is_ok(),
        "Cqo + Idiv should verify correctly"
    );
}

#[test]
fn test_physical_idiv_without_cqo_fails() {
    let program = make_program(vec![make_func(
        "bad",
        vec![],
        DtalType::Int,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::MovImm {
                    dst: lr(),
                    imm: 42,
                    ty: DtalType::Int,
                },
                DtalInstr::MovImm {
                    dst: r7(),
                    imm: 10,
                    ty: DtalType::Int,
                },
                DtalInstr::Idiv { src: r7() },
                DtalInstr::MovReg {
                    dst: r0(),
                    src: lr(),
                    ty: DtalType::Int,
                },
                DtalInstr::Ret,
            ],
        )],
    )]);
    let result = verify_dtal(&program);
    assert!(result.is_err(), "Idiv without R2 defined should fail");
}

#[test]
fn test_physical_spill_store_load() {
    let program = make_program(vec![make_func(
        "spill",
        vec![(r0(), DtalType::Int)],
        DtalType::Int,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::Prologue {
                    frame_size: 8,
                    callee_saved: vec![],
                },
                DtalInstr::SpillStore {
                    src: r0(),
                    offset: -8,
                    ty: DtalType::Int,
                },
                DtalInstr::SpillLoad {
                    dst: lr(),
                    offset: -8,
                    ty: DtalType::Int,
                },
                DtalInstr::Epilogue {
                    callee_saved: vec![],
                },
                DtalInstr::Ret,
            ],
        )],
    )]);
    assert!(
        verify_dtal(&program).is_ok(),
        "Spill store/load should verify"
    );
}

#[test]
fn test_physical_shared_borrow_spill_store_load() {
    let array_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(1),
    };
    let ref_ty = DtalType::Ref(Arc::new(array_ty.clone()));
    let program = make_program(vec![DtalFunction {
        name: "spill_shared_borrow".to_string(),
        params: vec![(r0(), array_ty)],
        parameter_kinds: vec![crate::common::ownership::ParameterKind::OwnedValue],
        return_type: DtalType::SingletonInt(IndexExpr::Const(0)),
        precondition: None,
        postcondition: None,
        blocks: vec![make_block(
            ".entry",
            vec![
                DtalInstr::Prologue {
                    frame_size: 8,
                    callee_saved: vec![],
                },
                DtalInstr::AliasBorrow {
                    lifetime: None,
                    dst: Reg::Physical(PhysicalReg::R1),
                    src: r0(),
                    ty: ref_ty.clone(),
                },
                DtalInstr::SpillStore {
                    src: Reg::Physical(PhysicalReg::R1),
                    offset: -8,
                    ty: ref_ty.clone(),
                },
                DtalInstr::SpillLoad {
                    dst: Reg::Physical(PhysicalReg::R3),
                    offset: -8,
                    ty: ref_ty.clone(),
                },
                DtalInstr::BorrowEnd {
                    lifetime: None,
                    src: Reg::Physical(PhysicalReg::R3),
                    ty: ref_ty,
                },
                DtalInstr::MovImm {
                    dst: lr(),
                    imm: 0,
                    ty: DtalType::SingletonInt(IndexExpr::Const(0)),
                },
                DtalInstr::Epilogue {
                    callee_saved: vec![],
                },
                DtalInstr::Ret,
            ],
        )],
    }]);
    assert!(
        verify_dtal(&program).is_ok(),
        "Physical shared-borrow spill store/load should verify"
    );
}

#[test]
fn test_physical_mutable_borrow_spill_store_load() {
    let array_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(1),
    };
    let ref_mut_ty = DtalType::RefMut(Arc::new(array_ty.clone()));
    let program = make_program(vec![DtalFunction {
        name: "spill_mut_borrow".to_string(),
        params: vec![(r0(), array_ty)],
        parameter_kinds: vec![crate::common::ownership::ParameterKind::OwnedValue],
        return_type: DtalType::SingletonInt(IndexExpr::Const(0)),
        precondition: None,
        postcondition: None,
        blocks: vec![make_block(
            ".entry",
            vec![
                DtalInstr::Prologue {
                    frame_size: 8,
                    callee_saved: vec![],
                },
                DtalInstr::BorrowMut {
                    lifetime: None,
                    dst: Reg::Physical(PhysicalReg::R1),
                    src: r0(),
                    ty: ref_mut_ty.clone(),
                },
                DtalInstr::SpillStore {
                    src: Reg::Physical(PhysicalReg::R1),
                    offset: -8,
                    ty: ref_mut_ty.clone(),
                },
                DtalInstr::SpillLoad {
                    dst: Reg::Physical(PhysicalReg::R3),
                    offset: -8,
                    ty: ref_mut_ty.clone(),
                },
                DtalInstr::BorrowEnd {
                    lifetime: None,
                    src: Reg::Physical(PhysicalReg::R3),
                    ty: ref_mut_ty,
                },
                DtalInstr::MovImm {
                    dst: lr(),
                    imm: 0,
                    ty: DtalType::SingletonInt(IndexExpr::Const(0)),
                },
                DtalInstr::Epilogue {
                    callee_saved: vec![],
                },
                DtalInstr::Ret,
            ],
        )],
    }]);
    assert!(
        verify_dtal(&program).is_ok(),
        "Physical mutable-borrow spill store/load should verify"
    );
}

#[test]
fn test_physical_spill_load_allows_more_specific_array_view() {
    let stored_ty = DtalType::Array {
        element_type: Arc::new(DtalType::Int),
        size: IndexExpr::Const(4),
    };
    let requested_ty = DtalType::Array {
        element_type: Arc::new(DtalType::SingletonInt(IndexExpr::Const(0))),
        size: IndexExpr::Const(4),
    };
    let program = make_program(vec![make_func(
        "spill_refine",
        vec![(r0(), stored_ty.clone())],
        DtalType::Unit,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::Prologue {
                    frame_size: 8,
                    callee_saved: vec![],
                },
                DtalInstr::SpillStore {
                    src: r0(),
                    offset: -8,
                    ty: stored_ty,
                },
                DtalInstr::SpillLoad {
                    dst: Reg::Physical(PhysicalReg::R1),
                    offset: -8,
                    ty: requested_ty,
                },
                DtalInstr::MovImm {
                    dst: lr(),
                    imm: 0,
                    ty: DtalType::Int,
                },
                DtalInstr::TypeAnnotation {
                    reg: lr(),
                    ty: DtalType::Unit,
                },
                DtalInstr::Epilogue {
                    callee_saved: vec![],
                },
                DtalInstr::Ret,
            ],
        )],
    )]);
    assert!(
        verify_dtal(&program).is_ok(),
        "Spill load should allow a more specific array view than the stored supertype"
    );
}

#[test]
fn test_physical_call_sets_lr() {
    let callee = make_func(
        "get_value",
        vec![],
        DtalType::Int,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::Prologue {
                    frame_size: 0,
                    callee_saved: vec![],
                },
                DtalInstr::MovImm {
                    dst: lr(),
                    imm: 99,
                    ty: DtalType::SingletonInt(IndexExpr::Const(99)),
                },
                DtalInstr::Epilogue {
                    callee_saved: vec![],
                },
                DtalInstr::Ret,
            ],
        )],
    );
    let caller = make_func(
        "main",
        vec![],
        DtalType::Int,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::Prologue {
                    frame_size: 0,
                    callee_saved: vec![],
                },
                DtalInstr::Call {
                    target: "get_value".to_string(),
                    arg_kinds: vec![],
                    return_ty: DtalType::Int,
                    ownership: crate::common::ownership::OwnershipMode::Plain,
                },
                DtalInstr::MovReg {
                    dst: r0(),
                    src: lr(),
                    ty: DtalType::Int,
                },
                DtalInstr::MovReg {
                    dst: lr(),
                    src: r0(),
                    ty: DtalType::Int,
                },
                DtalInstr::Epilogue {
                    callee_saved: vec![],
                },
                DtalInstr::Ret,
            ],
        )],
    );
    let program = make_program(vec![callee, caller]);
    assert!(
        verify_dtal(&program).is_ok(),
        "Physical call should set LR to return type"
    );
}

#[test]
fn test_physical_prologue_defines_scratch_regs() {
    let program = make_program(vec![make_func(
        "main",
        vec![],
        DtalType::Int,
        vec![make_block(
            ".entry",
            vec![
                DtalInstr::Prologue {
                    frame_size: 0,
                    callee_saved: vec![],
                },
                DtalInstr::MovImm {
                    dst: r7(),
                    imm: 10,
                    ty: DtalType::Int,
                },
                DtalInstr::BinOp {
                    op: crate::dtal::instr::BinaryOp::Add,
                    dst: lr(),
                    lhs: lr(),
                    rhs: r7(),
                    ty: DtalType::Int,
                },
                DtalInstr::Epilogue {
                    callee_saved: vec![],
                },
                DtalInstr::Ret,
            ],
        )],
    )]);
    assert!(
        verify_dtal(&program).is_ok(),
        "Prologue should define scratch registers"
    );
}

#[test]
fn test_i64_overflow_check_function() {
    let ctx = vec![
        Constraint::And(
            Box::new(Constraint::Ge(
                IndexExpr::Var("v0".into()),
                IndexExpr::Const(0),
            )),
            Box::new(Constraint::Le(
                IndexExpr::Var("v0".into()),
                IndexExpr::Const(1000),
            )),
        ),
        Constraint::And(
            Box::new(Constraint::Ge(
                IndexExpr::Var("v1".into()),
                IndexExpr::Const(0),
            )),
            Box::new(Constraint::Le(
                IndexExpr::Var("v1".into()),
                IndexExpr::Const(1000),
            )),
        ),
    ];
    let sum = IndexExpr::Add(
        Box::new(IndexExpr::Var("v0".into())),
        Box::new(IndexExpr::Var("v1".into())),
    );
    assert!(
        checker::check_i64_overflow_constraint(&sum, &ctx),
        "check_i64_overflow_constraint should pass for bounded operands"
    );
}

#[test]
fn test_i64_overflow_constraint_proof() {
    let ctx = vec![
        Constraint::And(
            Box::new(Constraint::Ge(
                IndexExpr::Var("v0".into()),
                IndexExpr::Const(0),
            )),
            Box::new(Constraint::Le(
                IndexExpr::Var("v0".into()),
                IndexExpr::Const(1000),
            )),
        ),
        Constraint::And(
            Box::new(Constraint::Ge(
                IndexExpr::Var("v1".into()),
                IndexExpr::Const(0),
            )),
            Box::new(Constraint::Le(
                IndexExpr::Var("v1".into()),
                IndexExpr::Const(1000),
            )),
        ),
    ];

    let sum = IndexExpr::Add(
        Box::new(IndexExpr::Var("v0".into())),
        Box::new(IndexExpr::Var("v1".into())),
    );

    let lower = Constraint::Ge(sum.clone(), IndexExpr::Const(i64::MIN as i128));
    let upper = Constraint::Le(sum, IndexExpr::Const(i64::MAX as i128));

    assert!(
        checker::is_constraint_provable(&lower, &ctx),
        "v0+v1 >= INT_MIN should be provable from v0,v1 in [0,1000]"
    );
    assert!(
        checker::is_constraint_provable(&upper, &ctx),
        "v0+v1 <= INT_MAX should be provable from v0,v1 in [0,1000]"
    );
}
